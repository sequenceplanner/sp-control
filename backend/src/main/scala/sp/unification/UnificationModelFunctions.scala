package sp.unification

import sp.abilityhandler.APIAbilityHandler
import sp.devicehandler.VD
import sp.domain._
import sp.domain.logic.{ActionParser, PropositionParser}
import sp.drivers.{HumanDriver, URDriver}

trait Modelfunctions {
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
      preCondition = makeCondition("pre", active.id.toString)(theThings),
      postCondition = makeCondition("post", s"${currentPos.id} = ${refPos.id}")(theThings),
      resetCondition = makeCondition("reset", "true", s"${refPos.id} := ${currentPos.id}")(theThings)
    )

    // example of a move with no parameters:
    val moveTo20 = APIAbilityHandler.Ability(
      name = "moveTo20",
      preCondition = makeCondition("pre", s"active AND ${refPos.id} == ${currentPos.id} AND ${currentPos.id} != 20", s"${refPos.id} := 20")(theThings),
      started = makeCondition("started", s"${refPos.id} = 20")(theThings),
      postCondition = makeCondition("post", s"${currentPos.id} = 20")(theThings),
      resetCondition = makeCondition("reset", "true", s"${refPos.id} := ${currentPos.id}")(theThings)
    )

    val enableRobot = APIAbilityHandler.Ability(
      name = s"$name.enable",
      parameters = List(),
      preCondition = makeCondition("pre", s"${active.id} = false", s"${active.id} := true")(theThings),
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

  def makeAHuman(name: String) = {
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
      preCondition = makeCondition("pre", "true")(theThings),
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
    Condition(g.get, xs.toList, attributes = SPAttributes("kind" -> kind))
  }
}