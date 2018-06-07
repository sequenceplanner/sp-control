// package sp.unification

// import akka.actor._
// import sp.abilityhandler.APIAbilityHandler
// import sp.devicehandler.VD.DriverStateMapper
// import sp.devicehandler._
// import sp.domain.Logic._
// import sp.domain._
// import sp.domain.logic.{ActionParser, PropositionParser}
// import sp.drivers.ROSFlatStateDriver
// import sp.models.{APIModel, APIModelMaker}
// import sp.runners._
// import sp.service.MessageBussSupport
// import sp.vdtesting.APIVDTracker
// import sp.abilityhandler.APIAbilityHandler.Ability

// import scala.concurrent.duration._

// trait Helpers {
//   def a(n:String, parameters: List[ID], pre:Condition, exec:Condition, post:Condition, reset:Condition=Condition(AlwaysTrue, List()),pairs: Map[String, SPValue] = Map()) = {
//     val p = pairs + ("name" -> SPValue(n))
//     Ability(n, ID.newID, pre, exec, post, reset, parameters, attributes = SPAttributes("pairs" -> p))
//   }
//   def v(name: String, drivername: String) = Thing(name, SPAttributes("drivername" -> drivername))
//   def makeCondition(kind: String, guard: String, actions: String*)(aList: List[IDAble]) = {
//     val g = if (guard == "true") Some(AlwaysTrue)
//     else if (guard == "false") Some(AlwaysFalse)
//     else PropositionParser(aList).parseStr(guard) match {
//       case Right(p) => Some(p)
//       case Left(err) => println(s"Parsing failed on condition: $guard: $err"); None
//     }
//     val xs = actions.flatMap { action =>
//       ActionParser(aList).parseStr(action) match {
//         case Right(a) => Some(a)
//         case Left(err) => println(s"Parsing failed on action: $action: $err"); None
//       }
//     }
//     Condition(g.get, xs.toList, attributes = SPAttributes("kind"->kind))
//   }

//   def driverMapper(driverID: ID, vars: List[Thing]): List[VD.OneToOneMapper] = vars.flatMap { v =>
//     v.attributes.getAs[String]("drivername").map(dn => VD.OneToOneMapper(v.id, driverID, dn))
//   }
// }

// object TurtleModel extends Helpers {
//   def props = Props(classOf[TurtleModel])

//   val name = "turtle"

//   val cmd_linear_x = v("turtle.cmd.linear.x", "geometry_msgs/Twist:/turtle1/cmd_vel:linear.x") // 250ms between writes
//   val cmd_linear_y = v("turtle.cmd.linear.y", "geometry_msgs/Twist:/turtle1/cmd_vel:linear.y")
//   val pos_x = v("turtle.pos.x", "turtlesim/Pose:/turtle1/pose:x")
//   val pos_y = v("turtle.pos.y", "turtlesim/Pose:/turtle1/pose:y")
//   val test_str = v("test_str", "std_msgs/String:/hecu_hca_unistate:data")
//   val test_mir = v("mir_x", "nav_msgs/Odometry:/mir100_diff_drive_controller/odom:pose.pose.position.x")
//   val test_control = v("test_control", "std_msgs/String:/sp_to_fake_mir_unidriver:data:100")

//   // ur robot state
//   val ur_act_pos = v("ur_act_pos", "unification_roscontrol/URPose1:/unification/ur_pose_unidriver/state:actPos")
//   val ur_executing = v("ur_executing", "unification_roscontrol/URPose1:/unification/ur_pose_unidriver/state:executing")
//   val ur_ref_posD = v("ur_ref_pos_driver", "unification_roscontrol/URPose1:/unification/ur_pose_unidriver/state:refPos")

//   // ur robot commands
//   val ur_ref_posSP = v("ur_ref_pos_sp", "unification_roscontrol/URPose2:/unification/ur_pose_unidriver/cmd:refPos:250")

//   val vars: List[Thing] = List(cmd_linear_x, cmd_linear_y, pos_x, pos_y, test_str, test_control, test_mir,
//     ur_act_pos, ur_executing, ur_ref_posD, ur_ref_posSP)

//   val driverID = ID.newID
//   val driverMap = driverMapper(driverID, vars)
//   // val driverSetup = SPAttributes("localHostname" -> "ubuntu",
//   //   "masterURI" -> "http://192.170.1.101:11311/", "identifiers" -> driverMap.map(_.driverIdentifier))
//   val driverSetup = SPAttributes("identifiers" -> driverMap.map(_.driverIdentifier))

//   val driver = VD.Driver("TurtleDriver", driverID, ROSFlatStateDriver.driverType, driverSetup)

//   val resource = VD.Resource(name, ID.newID, vars.map(_.id).toSet, driverMap, SPAttributes())

//   def p(kind: String, guard: String, actions: String*) =  makeCondition(kind,guard,actions:_*)(vars)

//   val urdummyabs = List("URDummyPose1", "URDummyPose2", "URDummyPose3", "URDummyPose4").map { pose =>
//     a("goto"+pose, List(),
//       p("pre", "true", s"ur_ref_pos_sp := '$pose'"),// s"ur_act_pos != '$pose'", s"ur_ref_pos_sp := '$pose'"),
//       p("started", s"ur_ref_pos_driver == '$pose' && ur_executing"), // note that we check the driver state
//       p("post", s"ur_act_pos == '$pose' && !ur_executing"),
//       p("reset", "true"))
//   }

//   val testControlOn = a("testRosControlOn", List(),
//     p("pre", "true", "test_control := move"),
//     p("started", "test_control == move"),
//     p("post", "true"),
//     p("reset", "true"))

//   val testControlOff = a("testRosControlOff", List(),
//     p("pre", "true", "test_control := dontmove"),
//     p("started", "test_control == dontmove"),
//     p("post", "true"),
//     p("reset", "true"))

//   val moveForward = a("turtle.moveForward", List(),
//     p("pre", "true", "turtle.cmd.linear.x := 5"),
//     p("started", "turtle.cmd.linear.x == 5", "turtle.cmd.linear.y := -5e3"),
//     p("post", "true"),
//     p("reset", "true"))

//   val moveBackward = a("turtle.moveBackward", List(),
//     p("pre", "true", "turtle.cmd.linear.x := -5"),
//     p("started", "turtle.cmd.linear.x == -5"),
//     p("post", "true", "turtle.cmd.linear.y := 0"),
//     p("reset", "true"))

//   val abilities = List(moveForward, moveBackward, testControlOn, testControlOff) ++ urdummyabs

//   // operations
//   val opPosX = Thing("PosX")
//   val opPosY = Thing("PosY")
//   // internal state
//   val opGoForward = Thing("goForward")


//   val initState: Map[ID, SPValue] = Map(
//     opPosX.id -> 0,
//     opPosY.id -> 0,
//     opGoForward.id -> true
//   )

//   val opvars = List(opPosX, opPosY, opGoForward)
//   def g(kind: String, guard: String, actions: String*) =  makeCondition(kind,guard,actions:_*)(opvars)

//   // should the op complete as soon as the ability completes (in this
//   // case, right away) or should it also wait for the post guard
//   // to become true?
//   val opMoveForward = Operation(name = "moveForward",
//     conditions =  List(
//       g("pre", "PosX < 1 && goForward"),
//       g("post", "PosX > 9", "goForward := false")))

//   val opMoveBackward = Operation(name = "moveBackward",
//     conditions =  List(
//       g("pre", "PosX > 9 && !goForward"),
//       g("post", "PosX < 1", "goForward := true")))

//   val opRunner = APIOperationRunner.Setup(
//     name = "TurtleRunner",
//     runnerID = ID.newID,
//     ops = Set(opMoveForward, opMoveBackward),
//     opAbilityMap = Map(opMoveForward.id -> moveForward.id, opMoveBackward.id -> moveBackward.id),
//     initialState = initState,
//     variableMap = Map(
//       opPosX.id -> pos_x.id,
//       opPosY.id -> pos_y.id
//     ),
//     abilityParameters = Map()
//   )
// }



// class TurtleModel extends Actor with MessageBussSupport{
//   import context.dispatcher

//   subscribe(APIModel.topicResponse)
//   subscribe(APIModelMaker.topicResponse)
//   subscribe(APIVDTracker.topicRequest)


//   import TurtleModel._



//   def launchVDAbilities(ids : List[IDAble])= {

//     // Extract model data from IDAbles
//     val ops = ids.filter(_.isInstanceOf[Operation]).map(_.asInstanceOf[Operation])
//     val things = ids.filter(_.isInstanceOf[Thing]).map(_.asInstanceOf[Thing])
//     val rTmp = things.filter(t => t.attributes.keys.contains("stateMap"))
//     val setupRunnerThings = things.filter(t => t.name == "setupRunnerAsThing")

//     val exAbilities = ops.flatMap(o=> APIAbilityHandler.operationToAbility(o))
//     val exResorces = rTmp.map(t => VD.thingToResource(t))
//     val exDrivers = things.diff(rTmp).diff(setupRunnerThings).map(t=> VD.thingToDriver(t))
//     val exSetupRunner = APIOperationRunner.CreateRunner(thingToSetup(setupRunnerThings.head))

//     //Direct launch of the VD and abilities below
//     val vdID = ID.newID
//     val abID = ID.newID

//     publish(APIVirtualDevice.topicRequest,
//       SPMessage.makeJson(
//         SPHeader(from = "UnificationAbilities"),
//         APIVirtualDevice.SetUpVD(
//           name = "UnificationVD",
//           id = vdID,
//           exResorces, //= resources.map(_.resource),
//           exDrivers, // = resources.map(_.driver),
//           attributes = SPAttributes()
//         )))

//     publish(APIAbilityHandler.topicRequest,
//       SPMessage.makeJson(
//         SPHeader(from = "UnificationAbilities"),
//         APIAbilityHandler.SetUpAbilityHandler(
//           name = "UnificationAbilites",
//           id = abID,
//           exAbilities,
//           vd = vdID
//         )))
//   }

//   def launchOpRunner(h: SPHeader, ids : List[IDAble])= {

//     // Extract setup data from IDAbles
//     val setupRunnerThings = ids.find{t =>
//       println(s"t: ${t.name}, isit: ${t.name == "setupRunnerAsThing" && t.isInstanceOf[Thing]}")
//       t.name == "setupRunnerAsThing" && t.isInstanceOf[Thing]}.map(_.asInstanceOf[Thing])

//     println(setupRunnerThings)


//     setupRunnerThings.map{s =>
//       val runnerSetup = thingToSetup(s).copy(runnerID = ID.newID)
//       val exSetupRunner = APIOperationRunner.CreateRunner(runnerSetup)

//       publish(APIOperationRunner.topicRequest, SPMessage.makeJson(
//         SPHeader(from = "UnificationAbilities", to = APIOperationRunner.service), exSetupRunner))

//       publish(APIVDTracker.topicResponse, SPMessage.makeJson(h, APIVDTracker.OpRunnerCreated(runnerSetup.runnerID)))

//     }

//   }


//   def saveModel() = {

//     //val modelID = ID.makeID("0d80d1d6-48cd-48ec-bfb1-d69714ef35be").get // hardcoded model id so we do not get a new model every time

//     val cm = sp.models.APIModelMaker.CreateModel("unificationROSVD", SPAttributes("isa" -> "VD"))

//     val abs = abilities.map(a=>APIAbilityHandler.abilityToOperation(a))
//     val rIDable = VD.resourceToThing(resource)
//     val dIDable = VD.driverToThing(driver)
//     val setup = setupToThing(opRunner)

//     val theVD = Struct(
//       "TheVD",
//       makeStructNodes(
//         rIDable,
//         dIDable,
//         setup
//       ) ++ makeStructNodes(abs),
//       SPAttributes("isa" -> "VD")
//     )
//     val xs = List(rIDable, dIDable,setup) ++ abs ++ vars ++ opvars ++ opRunner.ops

//     val addItems = APIModel.PutItems(theVD :: xs, SPAttributes("info" -> "initial items"))

//     context.system.scheduler.scheduleOnce(0.1 seconds) {
//       publish(
//         APIModelMaker.topicRequest,
//         SPMessage.makeJson(SPHeader(from = "UnificationAbilities", to = APIModelMaker.service), cm)
//       )
//     }

//     context.system.scheduler.scheduleOnce(0.2 seconds) {
//       publish(
//         APIModel.topicRequest,
//         SPMessage.makeJson(SPHeader(from = "UnificationAbilities", to = cm.id.toString), addItems)
//       )
//     }
//   }


//   def setupToThing(setup : APIOperationRunner.Setup): Thing = {
//     Thing(
//       name = "setupRunnerAsThing",
//       id = ID.newID,
//       attributes = SPAttributes(
//         "name" -> setup.name,
//         "runnerID" -> setup.runnerID,
//         "ops" -> setup.ops,
//         "opAbilityMap" -> setup.opAbilityMap,
//         "initialState" -> setup.initialState,
//         "variableMap" -> setup.variableMap,
//         "abilityParameters" -> setup.abilityParameters.toList
//       )
//     )
//   }

//   def thingToSetup(thing : Thing): APIOperationRunner.Setup = {
//     val name = thing.attributes.getAs[String]("name").getOrElse("")
//     val runnerID = thing.attributes.getAs[ID]("runnerID").getOrElse(ID.newID)
//     val ops = thing.attributes.getAs[Set[Operation]]("ops").getOrElse(Set())
//     val opAbilityMap = thing.attributes.getAs[Map[ID,ID]]("opAbilityMap").getOrElse(Map())
//     val initialState = thing.attributes.getAs[Map[ID,SPValue]]("initialState").getOrElse(Map())
//     val variableMap = thing.attributes.getAs[Map[ID,ID]]("variableMap").getOrElse(Map())
//     val abilityParameters = thing.attributes.getAs[List[(ID,Set[ID])]]("abilityParameters").getOrElse(List()).toMap
//     APIOperationRunner.Setup(name, runnerID,ops,opAbilityMap,initialState,variableMap,abilityParameters)
//   }

//   def receive = {
//     case s : String =>
//       for { // unpack message
//         mess <- SPMessage.fromJson(s)
//         h <- mess.getHeaderAs[SPHeader] if  h.to == APIVDTracker.service
//         b <- mess.getBodyAs[APIVDTracker.Request]
//       } yield {
//         val spHeader = h.swapToAndFrom()
//         sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK())) // acknowledge message received
//         b match { // Check if the body is any of the following classes, and execute program
//           case APIVDTracker.createModel(modelID) => saveModel()
//           case APIVDTracker.launchVDAbilities(idables) => launchVDAbilities(idables)
//           case APIVDTracker.launchOpRunner(idables) => launchOpRunner(spHeader,idables)
//         }
//         sendAnswer(SPMessage.makeJson(spHeader, APISP.SPDone()))
//       }
//   }
//   def sendAnswer(mess: String) = publish(APIVDTracker.topicResponse, mess)

// }
