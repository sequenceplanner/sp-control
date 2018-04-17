package sp.unification

import akka.actor._
import sp.abilityhandler.APIAbilityHandler
import sp.devicehandler.VD.DriverStateMapper
import sp.devicehandler._
import sp.domain.Logic._
import sp.domain._
import sp.domain.logic.{ActionParser, PropositionParser}
import sp.drivers.{HumanDriver, ROSDriver, URDriver}
import sp.models.{APIModel, APIModelMaker}
import sp.runners._
import sp.service.MessageBussSupport
import sp.vdtesting.APIVDTracker

import scala.concurrent.duration._


object UnificationROSModel {
  def props = Props(classOf[UnificationROSModel])

  // UR VD
  val name = "UR"
  val refPos = Thing(name = s"$name.refPos")
  val mode = Thing(name = s"$name.mode")
  val actPos = Thing(s"$name.actPos")
  //val hasTool = Thing(s"$name.hasTool") // can not change (currently we do not distinguish)
  val theThings: List[Thing] = List(refPos, mode, actPos)

  val driver = VD.Driver(s"ur_unidriver_2", ID.newID, ROSDriver.driverType, SPAttributes())

  val driverResourceMapper = List(
    VD.OneToOneMapper(refPos.id, driver.id, "refPos"),
    VD.OneToOneMapper(actPos.id, driver.id, "actPos"),
    VD.OneToOneMapper(mode.id, driver.id, "mode")
  )
  val ids: Set[ID] = theThings.map(_.id).toSet

  val resource = VD.Resource(name, ID.newID, ids, driverResourceMapper, SPAttributes())


  // The operation need to send refPos
  val command = APIAbilityHandler.Ability(
    name = s"$name.moveTo",
    parameters = List(refPos.id),
    preCondition = makeCondition("pre",s"${actPos.id} = ${refPos.id}")(theThings),
    started = makeCondition("started",s"${actPos.id} != ${refPos.id}")(theThings),
    postCondition = makeCondition("post", s"${actPos.id} = ${refPos.id}")(theThings),
    resetCondition = makeCondition("reset", "true")(theThings)
  )


  // UR OP
  val refPosOP = Thing(name = s"$name.refPosOP")
  val modeOP = Thing(name = s"$name.modeOP")
  val actPosOP = Thing(s"$name.actPosOP")
  val booked = Thing(s"$name.booked")

  val initState: Map[ID, SPValue] = Map(
    refPosOP.id -> "",
    actPosOP.id -> "",
    modeOP.id -> "NORMAL",
    booked.id -> false
  )

  val vars = List(refPosOP, modeOP, actPosOP, booked)

  val moveToHome = {
    Operation(
      name = "moveToHome",
      conditions =  List(
        // part is not taken, move (do not need to book due to the zone)
        makeCondition("pre",
          s"${booked.id} == false",
          s"${booked.id} := true")(vars),
        makeCondition("post",
          s"true",
          s"${booked.id} := true")(vars),
        makeCondition("reset", "true")(vars),

        // move the robot
        makeCondition("pre",
          s"${actPosOP.id} != URHomePos",
          s"${refPosOP.id} := URHomePos")(vars)
      )
    )
  }

  val moveToPickPos = {
    Operation(
      name = "moveToPickPos",
      conditions =  List(
        // part is not taken, move (do not need to book due to the zone)
        makeCondition("pre",
          s"${booked.id} == false",
          s"${booked.id} := true")(vars),
        makeCondition("post",
          s"true",
          s"${booked.id} := true")(vars),
        makeCondition("reset", "true")(vars),

        // move the robot
        makeCondition("pre",
          s"${actPosOP.id} != PickPos",
          s"${refPosOP.id} := PickPos")(vars)
      )
    )
  }


  val opRunner = APIOperationRunner.Setup(
    name = "ROS UR",
    runnerID = ID.newID,
    ops = Set(moveToHome, moveToPickPos),
    opAbilityMap = Map(moveToHome.id -> command.id, moveToPickPos.id -> command.id),
    initialState = initState,
    variableMap = Map(
      refPosOP.id -> refPos.id,
      actPosOP.id -> actPos.id,
      modeOP.id -> mode.id
    ),
    abilityParameters = Map(command.id -> Set(refPos.id))
  )




  def makeCondition(kind: String, guard: String, actions: String*)(aList: List[IDAble]) = {
    val g = if (guard == "true") Some(AlwaysTrue)
    else if (guard == "false") Some(AlwaysFalse)
    else PropositionParser(aList).parseStr(guard) match {
      case Right(p) => Some(p)
      case Left(err) => println(s"Parsing failed on condition: $guard: $err"); None
    }
    val xs = actions.flatMap { action =>
      ActionParser(aList).parseStr(action) match {
        case Right(a) => Some(a)
        case Left(err) => println(s"Parsing failed on action: $action: $err"); None
      }
    }
    Condition(g.get, xs.toList, attributes = SPAttributes("kind"->kind))
  }

}



class UnificationROSModel extends Actor with MessageBussSupport{
  import context.dispatcher

  subscribe(APIModel.topicResponse)
  subscribe(APIModelMaker.topicResponse)
  subscribe(APIVDTracker.topicRequest)


  import UnificationROSModel._



  def launchVDAbilities(ids : List[IDAble])= {

    // Extract model data from IDAbles
    val ops = ids.filter(_.isInstanceOf[Operation]).map(_.asInstanceOf[Operation])
    val things = ids.filter(_.isInstanceOf[Thing]).map(_.asInstanceOf[Thing])
    val rTmp = things.filter(t => t.attributes.keys.contains("stateMap"))
    val setupRunnerThings = things.filter(t => t.name == "setupRunnerAsThing")

    val exAbilities = ops.flatMap(o=> APIAbilityHandler.operationToAbility(o))
    val exResorces = rTmp.map(t => VD.thingToResource(t))
    val exDrivers = things.diff(rTmp).diff(setupRunnerThings).map(t=> VD.thingToDriver(t))
    val exSetupRunner = APIOperationRunner.CreateRunner(thingToSetup(setupRunnerThings.head))

    //Direct launch of the VD and abilities below
    val vdID = ID.newID
    val abID = ID.newID

    publish(APIVirtualDevice.topicRequest,
      SPMessage.makeJson(
        SPHeader(from = "UnificationAbilities"),
        APIVirtualDevice.SetUpVD(
          name = "UnificationVD",
          id = vdID,
          exResorces, //= resources.map(_.resource),
          exDrivers, // = resources.map(_.driver),
          attributes = SPAttributes()
        )))

    publish(APIAbilityHandler.topicRequest,
      SPMessage.makeJson(
        SPHeader(from = "UnificationAbilities"),
        APIAbilityHandler.SetUpAbilityHandler(
          name = "UnificationAbilites",
          id = abID,
          exAbilities,
          vd = vdID
        )))
  }

  def launchOpRunner(ids : List[IDAble])= {

    // Extract setup data from IDAbles
    val things = ids.filter(_.isInstanceOf[Thing]).map(_.asInstanceOf[Thing])
    val setupRunnerThings = things.filter(t => t.name == "setupRunnerAsThing")

    val exSetupRunner = APIOperationRunner.CreateRunner(thingToSetup(setupRunnerThings.head))

    //direct launch of operation runner
    publish(APIOperationRunner.topicRequest, SPMessage.makeJson(
      SPHeader(from = "UnificationAbilities", to = APIOperationRunner.service), exSetupRunner))
  }


  def saveModel() = {

    //val modelID = ID.makeID("0d80d1d6-48cd-48ec-bfb1-d69714ef35be").get // hardcoded model id so we do not get a new model every time

    val cm = sp.models.APIModelMaker.CreateModel("unificationROSVD", SPAttributes("isa" -> "VD"))

    val ops = APIAbilityHandler.abilityToOperation(command)
    val rIDable = VD.resourceToThing(resource)
    val dIDable = VD.driverToThing(driver)
    val setup = setupToThing(opRunner)

    val theVD = Struct(
      "TheVD",
      makeStructNodes(
        ops,
        refPos,
        actPos,
        mode,
        rIDable,
        dIDable,
        setup
      ),
      SPAttributes("isa" -> "VD")
    )
    val xs = List(rIDable, dIDable,setup) ++ vars ++ theThings ++ opRunner.ops

    val addItems = APIModel.PutItems(theVD :: List(ops) ++ xs, SPAttributes("info" -> "initial items"))

    context.system.scheduler.scheduleOnce(0.1 seconds) {
      publish(
        APIModelMaker.topicRequest,
        SPMessage.makeJson(SPHeader(from = "UnificationAbilities", to = APIModelMaker.service), cm)
      )
    }

    context.system.scheduler.scheduleOnce(0.2 seconds) {
      publish(
        APIModel.topicRequest,
        SPMessage.makeJson(SPHeader(from = "UnificationAbilities", to = cm.id.toString), addItems)
      )
    }
  }


  def setupToThing(setup : APIOperationRunner.Setup): Thing = {
    Thing(
      name = "setupRunnerAsThing",
      id = ID.newID,
      attributes = SPAttributes(
        "name" -> setup.name,
        "runnerID" -> setup.runnerID,
        "ops" -> setup.ops,
        "opAbilityMap" -> setup.opAbilityMap,
        "initialState" -> setup.initialState,
        "variableMap" -> setup.variableMap,
        "abilityParameters" -> setup.abilityParameters.toList
      )
    )
  }

  def thingToSetup(thing : Thing): APIOperationRunner.Setup = {
    val name = thing.attributes.getAs[String]("name").getOrElse("")
    val runnerID = thing.attributes.getAs[ID]("runnerID").getOrElse(ID.newID)
    val ops = thing.attributes.getAs[Set[Operation]]("ops").getOrElse(Set())
    val opAbilityMap = thing.attributes.getAs[Map[ID,ID]]("opAbilityMap").getOrElse(Map())
    val initialState = thing.attributes.getAs[Map[ID,SPValue]]("initialState").getOrElse(Map())
    val variableMap = thing.attributes.getAs[Map[ID,ID]]("variableMap").getOrElse(Map())
    val abilityParameters = thing.attributes.getAs[List[(ID,Set[ID])]]("abilityParameters").getOrElse(List()).toMap
    APIOperationRunner.Setup(name, runnerID,ops,opAbilityMap,initialState,variableMap,abilityParameters)
  }

  def receive = {
    case s : String =>
      for { // unpack message
        mess <- SPMessage.fromJson(s)
        h <- mess.getHeaderAs[SPHeader] if  h.to == APIVDTracker.service
        b <- mess.getBodyAs[APIVDTracker.Request]
      } yield {
        var spHeader = h.swapToAndFrom
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK())) // acknowledge message received
        b match { // Check if the body is any of the following classes, and execute program
          case APIVDTracker.createModel(modelID) => saveModel()
          case APIVDTracker.launchVDAbilities(idables) => launchVDAbilities(idables)
          case APIVDTracker.launchOpRunner(idables) => launchOpRunner(idables)
        }
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))
      }
  }
  def sendAnswer(mess: String) = publish(APIVDTracker.topicResponse, mess)

}
