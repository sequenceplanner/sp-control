package sp.unification

import akka.actor._
import sp.abilityhandler.APIAbilityHandler
import sp.devicehandler.VD.DriverStateMapper
import sp.devicehandler._
import sp.domain.Logic.{makeStructNodes, _}
import sp.domain._
import sp.domain.logic.{ActionParser, PropositionParser}
import sp.drivers.{HumanDriver, ROSDriver, URDriver}
import sp.models.{APIModel, APIModelMaker}
import sp.runners._
import sp.service.MessageBussSupport
import sp.vdtesting.APIVDTracker

import scala.concurrent.duration._


// Some support classes
case class RobotOperations(ops: List[Operation],
                           vars: List[Thing],
                           init: Map[ID, SPValue],
                           refID: ID,
                           current: ID,
                           activateID: ID
                          )
case class ResourceAndAbilities(abilities: List[APIAbilityHandler.Ability],
                                resource: VD.Resource,
                                driver: VD.Driver,
                                vars: List[Thing],
                                moveToID: ID,
                                refID: ID,
                                currentID: ID,
                                activateID: ID
                               )
object UnificationDummyVDModel {
  def props = Props(classOf[UnificationDummyVDModel])

  /** The operation model
    * The operations can move at the same time if they do not share zone and robot
    * In this model, we do not match but have hardcoded abilities and variables
    *
    */

  // shared state:
  val zoneParts = Thing("zoneParts") // Boolean
  val zoneOut = Thing("zoneOut") // Boolean

  val partAtA = Thing("partAtA") // partType, "empty" if no part
  val partAtB = Thing("partAtB") // partType, empty if no part
  val partAtC = Thing("partAtC") // partType, empty if no part
  val partAtD = Thing("partAtD") // partType, empty if no part
  val partAtOut = Thing("partAtOut") // partType, empty if no part

  val humanCmd = Thing("humanCmd") //

  val initStateShared: Map[ID, SPValue] = Map(
    zoneParts.id -> false,
    zoneOut.id -> false,
    partAtA.id -> "partA",
    partAtB.id -> "partB",
    partAtC.id -> "partC",
    partAtD.id -> "partD",
    partAtOut.id -> "empty",
    humanCmd.id -> ""
  )

  val modelVariables = List(zoneParts, zoneOut, partAtA, partAtB, partAtC, partAtD, partAtOut, humanCmd)

  val r1 = makeMeADummyRobotWithMoveAbility("r1")
  val r2 = makeMeADummyRobotWithMoveAbility("r2")
  val human = makeAHuman("kristofer")

  val parts = List(partAtA, partAtB, partAtC, partAtD)
  val resources = List(r1, r2, human)

  val humanOP = Operation(
    name = "humanUnload",
    conditions = List(
      makeCondition("pre", s"${partAtOut.id} != empty AND zoneOut == false", s"${humanCmd.id} := partAtOut", "zoneOut := true")(modelVariables),
      makeCondition("post", s"true", s"${partAtOut.id} := empty", "zoneOut := false")(modelVariables),
      makeCondition("reset", s"true")(modelVariables),
    )
  )


  val robotOperations: List[RobotOperations] = List(r1, r2).map{robot =>
    val resourceName = robot.resource.name
    // variables only used by the resource operations
    val ref = Thing(s"$resourceName.ref")
    val current = Thing(s"$resourceName.current")
    val booked = Thing(s"$resourceName.booked")
    val inTool = Thing(s"$resourceName.inTool")

    val initState: Map[ID, SPValue] = Map(
      ref.id -> 0,
      current.id -> 0,
      booked.id -> false,
      inTool.id -> "empty"
    )

    def makePick(name: String, fromP: Thing, toV: Int) = {
      makeMovePartOP(
        name = name, bookings = List(zoneParts, booked),
        fromPos = fromP, toPos = inTool,
        resourcePos = ref, toValue = toV)
    }

    val pickAtA = makePick(s"$resourceName.PickAtA", partAtA, 10)
    val pickAtB = makePick(s"$resourceName.PickAtB", partAtB, 20)
    val pickAtC = makePick(s"$resourceName.PickAtC", partAtC, 30)
    val pickAtD = makePick(s"$resourceName.PickAtD", partAtD, 40)
    val place = makeMovePartOP(
      name = s"$resourceName.place", bookings = List(zoneOut, booked),
      fromPos = inTool, toPos = partAtOut,
      resourcePos = ref, toValue = 60, reset = true)


    val actOp = Operation(s"$resourceName.activate", List(
      Condition(AlwaysTrue, List(), attributes = SPAttributes("kind"->"pre"))
    ))

    RobotOperations(
      ops = List(pickAtA, pickAtB, pickAtC, pickAtD, place, actOp),
      vars = List(ref, current, booked, inTool),
      init = initState,
      refID = ref.id,
      current = current.id,
      activateID = actOp.id
    )
  }

  // Setting up the hard coded mapping between operations and abilities
  val mappingOperationsAndAbilities = List(r1, r2) zip robotOperations map {
    case (r, robOp) =>
      val opAbMap = robOp.ops.map(_.id -> r.moveToID).toMap
      val activMap = Map(robOp.activateID -> r.activateID)
      val varMap = Map(
        robOp.refID -> r.refID,
        robOp.current -> r.currentID,
      )
      val abPar = Map(r.moveToID -> Set(robOp.refID))
      (opAbMap ++ activMap, varMap, abPar)
  }

  val humanOpAb = Map(humanOP.id -> human.abilities.head.id)
  val humanVMap = Map(humanCmd.id ->human.refID)
  val humanParam = Map(human.abilities.head.id -> Set(humanCmd.id))

  // Setting up the operation runner
  val setupRunner = APIOperationRunner.CreateRunner(APIOperationRunner.Setup(
    name = "test",
    runnerID = ID.newID,
    ops = robotOperations.flatMap(_.ops).toSet + humanOP,
    opAbilityMap = mappingOperationsAndAbilities.flatMap(_._1).toMap ++ humanOpAb,
    initialState = robotOperations.flatMap(_.init).toMap ++ initStateShared,
    variableMap = mappingOperationsAndAbilities.flatMap(_._2).toMap ++ humanVMap,
    abilityParameters = mappingOperationsAndAbilities.flatMap(_._3).toMap ++ humanParam
  ))




  def makeMovePartOP(name: String,
                     bookings: List[Thing],
                     fromPos: Thing,
                     toPos: Thing,
                     resourcePos: Thing,
                     toValue: Int,
                     reset: Boolean = false
                    ) = {
    val vars = fromPos :: toPos :: toPos :: resourcePos :: bookings

    val bs = bookings.flatMap(b => List(
      makeCondition("pre", s"${b.id} = false", s"${b.id} := true")(vars),
      makeCondition("post", "true", s"${b.id} := false")(vars)
    ))

    val resetCond: List[Condition] = if (reset) List(makeCondition("reset", "true")(vars)) else List()

    Operation(
      name = name,
      conditions = bs ++ resetCond ++ List(
        // part is not taken, move (do not need to book due to the zone)
        makeCondition("pre",
          s"${fromPos.id} != empty AND ${toPos.id} == empty",
          s"${toPos.id} := ${fromPos.id}")(vars),
        makeCondition("post", "true", s"${fromPos.id} := empty")(vars),

        // move the robot
        makeCondition("pre", "true", s"${resourcePos.id} := $toValue")(vars)
      )
    )
  }


  // Makes a dummy robot with one driver per robot.
  // Also includes the moveAbility
  def makeMeADummyRobotWithMoveAbility(name: String): ResourceAndAbilities = {
    val refPos = Thing(name = s"$name.refPos")
    val active = Thing(name = s"$name.active")
    val currentPos = Thing(s"$name.currentPos")
    //val hasTool = Thing(s"$name.hasTool") // can not change (currently we do not distinguish)
    val theThings: List[Thing] = List(refPos, active, currentPos)

    val driver = VD.Driver(s"$name-Driver", ID.newID, URDriver.driverType, SPAttributes())

    val driverResourceMapper = List(
      VD.OneToOneMapper(refPos.id, driver.id, "refPos"),
      VD.OneToOneMapper(active.id, driver.id, "active"),
      VD.OneToOneMapper(currentPos.id, driver.id, "currentPos")
    )
    val ids: Set[ID] = theThings.map(_.id).toSet

    val resource = VD.Resource(name, ID.newID, ids, driverResourceMapper, SPAttributes())


    // The operation need to send refPos
    val moveTo = APIAbilityHandler.Ability(
      name = s"$name.moveTo",
      parameters = List(refPos.id),
      preCondition = makeCondition("pre",active.id.toString)(theThings),
      postCondition = makeCondition("post", s"${currentPos.id} = ${refPos.id}")(theThings),
      resetCondition = makeCondition("reset", "true", s"${refPos.id} := ${currentPos.id}")(theThings)
    )

    // example of a move with no parameters:
    val moveTo20 = APIAbilityHandler.Ability(
      name = "moveTo20",
      preCondition = makeCondition("pre",s"active AND ${refPos.id} == ${currentPos.id} AND ${currentPos.id} != 20", s"${refPos.id} := 20")(theThings),
      started = makeCondition("started", s"${refPos.id} = 20")(theThings),
      postCondition = makeCondition("post", s"${currentPos.id} = 20")(theThings),
      resetCondition = makeCondition("reset", "true", s"${refPos.id} := ${currentPos.id}")(theThings)
    )

    val enableRobot = APIAbilityHandler.Ability(
      name = s"$name.enable",
      parameters = List(),
      preCondition = makeCondition("pre",s"${active.id} = false", s"${active.id} := true")(theThings),
      postCondition = makeCondition("post", s"${active.id} = true")(theThings)
    )

    ResourceAndAbilities(
      abilities = List(moveTo, enableRobot),
      resource = resource,
      driver = driver,
      vars = theThings,
      moveToID = moveTo.id,
      refID = refPos.id,
      currentID = currentPos.id,
      activateID = enableRobot.id
    )
  }

  def makeAHuman(name:String) = {
    val cmd = Thing(name = s"$name.cmd")
    val ack = Thing(name = s"$name.ack")
    val completed = Thing(name = s"$name.completed")
    val theThings: List[Thing] = List(cmd, ack, completed)
    val driver = VD.Driver(s"$name", ID.newID, HumanDriver.driverType, SPAttributes())
    val driverResourceMapper = List(
      VD.OneToOneMapper(cmd.id, driver.id, "cmd"),
      VD.OneToOneMapper(ack.id, driver.id, "ack"),
      VD.OneToOneMapper(completed.id, driver.id, "completed")
    )
    val ids: Set[ID] = theThings.map(_.id).toSet
    val resource = VD.Resource(name, ID.newID, ids, driverResourceMapper, SPAttributes())

    // The operation need to send cmd
    val humanInstruction = APIAbilityHandler.Ability(
      name = s"$name.instruction",
      parameters = List(cmd.id),
      preCondition = makeCondition("pre","true")(theThings),
      started = makeCondition("started", s"${completed.id} = false")(theThings),
      postCondition = makeCondition("post", s"${completed.id} = true")(theThings)
    )

    ResourceAndAbilities(
      abilities = List(humanInstruction),
      resource = resource,
      driver = driver,
      vars = theThings,
      moveToID = humanInstruction.id,
      refID = cmd.id,
      currentID = ack.id,
      activateID = completed.id
    )

  }


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



class UnificationDummyVDModel extends Actor with MessageBussSupport{
  import context.dispatcher

  subscribe(APIModel.topicResponse)
  subscribe(APIModelMaker.topicResponse)
  subscribe(APIVDTracker.topicRequest)


  import UnificationDummyVDModel._



  def launchVDAbilities(ids : List[IDAble])= {

    // Extract model data from IDAbles
    val ops = ids.filter(_.isInstanceOf[Operation]).map(_.asInstanceOf[Operation])
    val things = ids.filter(_.isInstanceOf[Thing]).map(_.asInstanceOf[Thing])
    val rTmp = things.filter(t => t.attributes.keys.contains("stateMap"))
    val setupRunnerThings = things.filter(t => t.name == "setupRunnerAsThing")

    val exAbilities = ops.map(o=> APIAbilityHandler.operationToAbility(o))
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

    val cm = sp.models.APIModelMaker.CreateModel("unificationVD", SPAttributes("isa" -> "VD"))

    val ops = resources.flatMap(_.abilities).map(APIAbilityHandler.abilityToOperation)
    val rIDable = resources.map(r => VD.resourceToThing(r.resource))
    val dIDable = resources.map(r => VD.driverToThing(r.driver))
    val vars = resources.map(r=> r.vars).flatten
    val setup = setupToThing(setupRunner.setup)

    val xs = rIDable ++ dIDable ++ vars :+ setup
    val theVD = Struct(
      "TheVD",
      makeStructNodes(dIDable.map(StructWrapper): _*)
        ++ makeStructNodes(vars.map(StructWrapper): _*)
        ++ makeStructNodes(rIDable.map(StructWrapper): _*)
        ++ makeStructNodes(ops.map(StructWrapper): _*)
        ++ makeStructNodes(List(setup).map(StructWrapper): _*),

      SPAttributes("isa" -> "VD")
    )

    val addItems = APIModel.PutItems(theVD :: ops ++ xs, SPAttributes("info" -> "initial items"))

    context.system.scheduler.scheduleOnce(1 seconds) {
      publish(
        APIModelMaker.topicRequest,
        SPMessage.makeJson(SPHeader(from = "UnificationAbilities", to = APIModelMaker.service), cm)
      )
    }

    context.system.scheduler.scheduleOnce(1.1 seconds) {
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
