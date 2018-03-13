package sp.unification

import akka.actor._
import sp.abilityhandler.APIAbilityHandler
import sp.devicehandler._
import sp.domain.Logic._
import sp.domain._
import sp.domain.logic.{ActionParser, PropositionParser}
import sp.drivers.{ROSDriver, URDriver}
import sp.models.{APIModel, APIModelMaker}
import sp.runners._
import sp.service.MessageBussSupport

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

  val initStateShared: Map[ID, SPValue] = Map(
    zoneParts.id -> false,
    zoneOut.id -> false,
    partAtA.id -> "partA",
    partAtB.id -> "partB",
    partAtC.id -> "partC",
    partAtD.id -> "partD",
    partAtOut.id -> "empty"
  )

  val modelVariables = List(zoneParts, zoneOut, partAtA, partAtB, partAtC, partAtD, partAtOut)

  val r1 = makeMeADummyRobotWithMoveAbility("r1")
  val r2 = makeMeADummyRobotWithMoveAbility("r2")

  val parts = List(partAtA, partAtB, partAtC, partAtD)
  val resources = List(r1, r2)

  val sinkOpOfParts = Operation(
    name = "sink",
    conditions = List(
      makeCondition("pre", s"${partAtOut.id} != empty", s"${partAtOut.id} := empty")(modelVariables),
      makeCondition("reset", s"true")(modelVariables),
    )
  )


  val robotOperations: List[RobotOperations] = resources.map{robot =>
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
  val mappingOperationsAndAbilities = resources zip robotOperations map {
    case (r, robOp) =>
      val opAbMap = robOp.ops.map(_.id -> r.moveToID).toMap
      val activMap = Map(robOp.activateID -> r.activateID)
      val varMap = Map(
        robOp.refID -> r.refID,
        robOp.current -> r.currentID
      )
      val abPar = Map(r.moveToID -> Set(robOp.refID))
      (opAbMap ++ activMap, varMap, abPar)
  }

  // Setting up the operation runner
  val setupRunner = APIOperationRunner.CreateRunner(APIOperationRunner.Setup(
    name = "test",
    runnerID = ID.newID,
    ops = robotOperations.flatMap(_.ops).toSet + sinkOpOfParts,
    opAbilityMap = mappingOperationsAndAbilities.flatMap(_._1).toMap,
    initialState = robotOperations.flatMap(_.init).toMap ++ initStateShared,
    variableMap = mappingOperationsAndAbilities.flatMap(_._2).toMap,
    abilityParameters = mappingOperationsAndAbilities.flatMap(_._3).toMap
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
          s"${fromPos.id} != empty && ${toPos.id} = empty",
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
      postCondition = makeCondition("post", s"${currentPos.id} = ${refPos.id}")(theThings)
    )

    // example of a move with no parameters:
    val moveTo20 = APIAbilityHandler.Ability(
      name = "moveTo20",
      preCondition = makeCondition("pre",s"active && ${refPos.id} == ${currentPos.id} && ${currentPos.id} != 20", s"${refPos.id} := 20")(theThings),
      started = makeCondition("started", s"${refPos.id} = 20")(theThings),
      postCondition = makeCondition("post", s"${currentPos.id} = 20")(theThings),
      resetCondition = makeCondition("reset", "true", s"${refPos.id} = ${currentPos.id}")(theThings)
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



  import UnificationDummyVDModel._




   //Direct launch of the VD and abilities below
  val vdID = ID.newID
  val abID = ID.newID

  publish(APIVirtualDevice.topicRequest,
    SPMessage.makeJson(
      SPHeader(from = "UnificationAbilities"),
      APIVirtualDevice.SetUpVD(
        name = "UnificationVD",
        id = vdID,
        resources = resources.map(_.resource),
        drivers = resources.map(_.driver),
        attributes = SPAttributes()
      )))

  publish(APIAbilityHandler.topicRequest,
    SPMessage.makeJson(
      SPHeader(from = "UnificationAbilities"),
      APIAbilityHandler.SetUpAbilityHandler(
        name = "UnificationAbilites",
        id = abID,
        abilities = resources.flatMap(_.abilities),
        vd = vdID
      )))


     //direct launch of operation runner
  publish(APIOperationRunner.topicRequest, SPMessage.makeJson(
    SPHeader(from = "UnificationAbilities", to=APIOperationRunner.service), setupRunner))









  // val VDModel = Struct(
  //   "VD",
  //   // resources.flatMap(_.abilities).map(APIAbilityHandler.abilityToOperation).map(
  //   //   op => StructNode(
  //   //       stuff here
  //   //   )
  //   ),
  //   SPAttributes("isa" -> "VD")

  val ops = resources.flatMap(_.abilities).map(APIAbilityHandler.abilityToOperation)
  val cm = sp.models.APIModelMaker.CreateModel("unificationVD", SPAttributes("isa"->"VD"))

  val rIDable = resources.map(r => VD.resourceToThing(r.resource))
  val dIDable = resources.map(r => VD.driverToThing(r.driver))

  val xs = rIDable ++ dIDable
  val theVD = Struct(
    "TheVD",
     makeStructNodes(dIDable.map(StructWrapper):_*)
     ++ makeStructNodes(rIDable.map(StructWrapper):_*)
     ++ makeStructNodes(ops.map(StructWrapper):_*),

    SPAttributes("isa"->"VD")
  )

  val addItems = APIModel.PutItems(theVD ::  ops ++ xs , SPAttributes("info"->"initial items"))
  
  context.system.scheduler.scheduleOnce(1 seconds){
    println("GOO")
    publish(
      APIModelMaker.topicRequest,
      SPMessage.makeJson(SPHeader(from = "UnificationAbilities", to = APIModelMaker.service), cm)
    )
  }

  context.system.scheduler.scheduleOnce(1.1 seconds){
    println("Goo 2")
    publish(
      APIModel.topicRequest,
      SPMessage.makeJson(SPHeader(from = "UnificationAbilities", to = cm.id.toString), addItems)
    )
  }

  


  // Setting up the model
  // Fix this soon
  //val modelID = ID.makeID("0d80d1d6-48cd-48ec-bfb1-d69714ef35be").get // hardcoded model id so we do not get a new model every time
  // creating a model here

  //  val ops = abs.map(APIAbilityHandler.abilityToOperation)
  //  val rIDable = VD.resourceToThing(resource)
  //  val dIDAble = VD.driverToThing(driver)
  //  val xs = things ++ List(rIDable, dIDAble)
  //  val theVD = Struct(
  //    "TheVD",
  //    makeStructNodes(
  //      rIDable.children(
  //        refPos, active, hasTool, currentPos
  //      ),
  //      dIDAble.children()
  //    ) ++ makeStructNodes(ops.map(StructWrapper):_*),
  //    SPAttributes("isa"->"VD")
  //  )
  //  // Probably add it to a struct as well
  //
  //  val cm = sp.models.APIModelMaker.CreateModel("unificationVD", SPAttributes("isa"->"VD"))
  //  val addItems = APIModel.PutItems(theVD :: ops ++ xs , SPAttributes("info"->"initial items"))



  //  context.system.scheduler.scheduleOnce(1 seconds){
  //    println("GOO")
  //    publish(APIModelMaker.topicRequest, SPMessage.makeJson(SPHeader(from = "UnificationAbilities", to = APIModelMaker.service), cm))
  //  }
  //  context.system.scheduler.scheduleOnce(1.1 seconds){
  //    println("Goo 2")
  //    publish(APIModel.topicRequest,SPMessage.makeJson(SPHeader(from = "UnificationAbilities", to = cm.id.toString), addItems))}












  // Not doing anything, creates the model on startup
  def receive = {
    case x => //println("Ability creation for unification got: "+x)
  }

}
