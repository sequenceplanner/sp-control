package sp.unification

import akka.actor.Props
import sp.domain.Logic._
import sp.domain._
import sp.runners.APIOperationRunner

// setup of model:

object UnificationDummyVDModelCopy { // used to set up the model as a service, add an entry in UnificationModelsLaunch as well
  object model extends Modelfunctions  {

  val name = "DummyVDCopy" // Model name
  val tags = List("") // Model tags

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


  val robotOperations: List[RobotOperations] = List(r1, r2).map { robot =>
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
      Condition(AlwaysTrue, List(), attributes = SPAttributes("kind" -> "pre"))
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
  val humanVMap = Map(humanCmd.id -> human.refID)
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

}
  def props = Props(UnificationModelsComSupport(model.name ,model.tags, model.resources, model.setupRunner))
}

