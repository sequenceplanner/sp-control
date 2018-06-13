package sp.runners

import akka.actor._
import akka.testkit._
import com.typesafe.config._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import sp.domain.Logic._
import sp.domain._
import sp.domain.logic.{ActionParser, PropositionParser}
import sp.runners.{APIOperationRunner => api}


/**
  * Created by kristofer on 2016-05-04.
  */
class OperationRunnerLogicTest(_system: ActorSystem) extends TestKit(_system) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll with Parsing  {

  def this() = this(ActorSystem("myTest", ConfigFactory.parseString(
    """
      |akka.loglevel = INFO
    """.stripMargin)))

  val p = TestProbe()
  val e = TestProbe()
  //val sh = system.actorOf(OperatorService.props(p.ref), "OperatorService")

  override def beforeAll: Unit = {

  }

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }




  "Op runner logic" must {

    val t1 = Thing("t1")
    val t2 = Thing("t2")
    val o1x = Operation("o1")
    val o2x = Operation("o2")
    val o3x = Operation("o3")
    val a1 = Thing("a1")
    val a2 = Thing("a2")
    val a3 = Thing("a3")


    val ids = List(t1, t2, o1x, o2x, o3x)
    val o1Pre = prop(ids, "t1 == 0", List("t1 := 1"))
    val o1Post = prop(ids, "", List("t1 := 2"), "post")

    val o2Pre = prop(ids, "o1 == f", List("t2 := 1"))
    val o3Pre = prop(ids, "t1 == 2", List("t1 := 3"))

    val o1 = o1x.copy(conditions = List(o1Pre, o1Post))
    val o2 = o2x.copy(conditions = List(o2Pre))
    val o3 = o3x.copy(conditions = List(o3Pre))



    val init = ids.collect{
      case x: Thing => x.id -> SPValue(0)
      case x: Operation => x.id -> SPValue("i")
    } toMap

    val abOp = Map(
      o1.id -> a1.id,
      o2.id -> a2.id,
      o3.id -> a3.id
    )

    val setup = api.Setup("r1", ID.newID, Set(o1, o2, o3), abOp, init)

    val initState = SPState(state = init)
    val ops = Set(o1, o2, o3)


    "do correct condition filtering" in {
      val test1 = prop(ids, "o1 == i", List(), "pre", "goodGroup")
      val test2 = prop(ids, "t1 == 2", List(), "post", "badGroup")
      val test3 = prop(ids, "t1 == 2", List(), "post", "goodGroup")

      val logic = new OperationRunnerLogic{def log = akka.event.Logging.getLogger(system, this)}

      val xs = List(test1, test2, test3)

      logic.filterConditions(xs, Set(), Set()) shouldEqual xs
      logic.filterConditions(xs, Set("pre"), Set()) shouldEqual List(test1)
      logic.filterConditions(xs, Set(), Set("badGroup")) shouldEqual List(test1, test3)

    }


    "evaluate ops" in {
      val logic = new OperationRunnerLogic{def log = akka.event.Logging.getLogger(system, this)}
      val res = logic.evaluateOps(List(o1, o2, o3), initState)
      assert(res == List(o1))

      var upd = logic.runOp(o1, initState)
      assert(upd(o1.id) == SPValue("e"))
      assert(upd(t1.id) == SPValue(1))

      val upd2 = logic.completeOP(o1, upd)
      assert(upd2(o1.id) == SPValue("f"))
      assert(upd2(t1.id) == SPValue(2))

      val res2 = logic.evaluateOps(List(o1, o2, o3), upd2)
      assert(res2 == List(o2, o3))
    }

    "evaluate ops with groupds" in {
      val logic = new OperationRunnerLogic{def log = akka.event.Logging.getLogger(system, this)}

      val o1Pre2 = prop(ids, "t1 == 1", List("t1 := 2"), "pre", "badGroup")
      val o1Upd = o1.copy(conditions = o1.conditions :+ o1Pre2)

      val res = logic.evaluateOps(List(o1Upd, o2, o3), initState)
      assert(res == List())

      val res2 = logic.evaluateOps(List(o1Upd, o2, o3), initState, Set("badGroup"))
      assert(res2 == List(o1Upd))


    }

    "upd state" in {
      val logic = new OperationRunnerLogic{def log = akka.event.Logging.getLogger(system, this)}
      logic.addRunner(setup)
      val r = logic.runners(setup.runnerID)
      val s = initState.add(Map[ID, SPValue](o1.id -> "f", t1.id -> 2))

      val res = logic.evaluateOps(List(o1, o2, o3), s)
      assert(res == List(o2, o3))

      var starting = List[ID]()
      val f = (o: ID, map: Map[ID, SPValue]) => starting = o :: starting

      var states = List[SPState]()
      val f2 = (o: SPState) => states = o :: states

      val upd = logic.newState(s, ops, r, f,  f2, true, Set(), None)
      println("jhsfd")
      println(upd)
      println(starting)
      println(states)
      assert(starting == List(a3.id, a2.id))
      assert(upd(t1.id) == SPValue(3) && upd(t2.id) == SPValue(1) &&
        upd(o2.id) == SPValue("e") && upd(o3.id) == SPValue("e"))

    }


    "run ops, all abilities" in {
      val logic = new OperationRunnerLogic{def log = akka.event.Logging.getLogger(system, this)}
      logic.addRunner(setup)


      var starting = List[ID]()
      val f = (o: ID, map: Map[ID, SPValue]) => starting = o :: starting

      var states = List[SPState]()
      val f2 = (o: SPState, id: ID) => states = o :: states

      logic.setRunnerState(setup.runnerID, initState, f, f2(_, setup.runnerID))

      logic.newAbilityState(a1.id, SPValue("enabled"), f, f2)
      logic.newAbilityState(a2.id, SPValue("enabled"), f, f2)
      logic.newAbilityState(a3.id, SPValue("enabled"), f, f2)

      logic.newAbilityState(a1.id, SPValue("finished"), f, f2)
      logic.newAbilityState(a2.id, SPValue("finished"), f, f2)
      logic.newAbilityState(a3.id, SPValue("finished"), f, f2)

      //logic.tickRunner(setup.runnerID, f, f2(_, setup.runnerID))


      //println("sfdsdf")
      //println(starting)
      //println(states)

      starting shouldEqual List(a3.id, a2.id, a1.id)
      states.head.get(o3.id).get shouldEqual  SPValue(OperationState.finished)
    }


    "run ops, one ability" in {
      val logic = new OperationRunnerLogic{def log = akka.event.Logging.getLogger(system, this)}
      val newS = setup.copy(opAbilityMap = Map(o1.id -> a1.id))
      logic.addRunner(newS)


      var starting = List[ID]()
      val f = (o: ID, map: Map[ID, SPValue]) => starting = o :: starting

      var states = List[SPState]()
      val f2 = (o: SPState, id: ID) => states = o :: states

      logic.setRunnerState(setup.runnerID, initState, f, f2(_, setup.runnerID))

      logic.newAbilityState(a1.id, SPValue("enabled"), f, f2)
      logic.newAbilityState(a1.id, SPValue("finished"), f, f2)

      logic.tickRunner(setup.runnerID, f, f2(_, setup.runnerID))


      //println("sfdsdf")
      //println(starting)
      //println(states)

      starting shouldEqual List(a1.id)
      states.head.get(o3.id).get shouldEqual  SPValue(OperationState.finished)
    }

    "run op in manual" in {
      val logic = new OperationRunnerLogic{def log = akka.event.Logging.getLogger(system, this)}
      val newS = setup.copy(opAbilityMap = Map(o1.id -> a1.id))
      logic.addRunner(newS)


      var starting = List[ID]()
      val f = (o: ID, map: Map[ID, SPValue]) => starting = o :: starting

      var states = List[SPState]()
      val f2 = (o: SPState, id: ID) => states = o :: states

      logic.setRunnerState(setup.runnerID, initState, f, f2(_, setup.runnerID))

      logic.newAbilityState(a1.id, SPValue("enabled"), f, f2)
      logic.newAbilityState(a1.id, SPValue("finished"), f, f2)

      logic.tickRunner(setup.runnerID, f, f2(_, setup.runnerID))


      //println("sfdsdf")
      //println(starting)
      //println(states)

      starting shouldEqual List(a1.id)
      states.head.get(o3.id).get shouldEqual  SPValue(OperationState.finished)
    }


    "run ops, reset and no ability" in {
      val logic = new OperationRunnerLogic{def log = akka.event.Logging.getLogger(system, this)}

      val resetC = Condition(AlwaysTrue, List(), SPAttributes("kind"->"reset"))
      val updO3 = o3.copy(conditions = o3.conditions :+ resetC)


      val newS = setup.copy(
        opAbilityMap = Map(),
        ops = Set(o1, o2, updO3)
      )

      logic.addRunner(newS)


      var starting = List[ID]()
      val f = (o: ID, map: Map[ID, SPValue]) => starting = o :: starting

      var states = List[SPState]()
      val f2 = (o: SPState, id: ID) => states = o :: states

      logic.setRunnerState(setup.runnerID, initState, f, f2(_, setup.runnerID))

      logic.tickRunner(setup.runnerID, f, f2(_, setup.runnerID))
      logic.tickRunner(setup.runnerID, f, f2(_, setup.runnerID))
      logic.tickRunner(setup.runnerID, f, f2(_, setup.runnerID))


      //println("sfdsdf")
      //println(starting)
      //println(states)

      starting shouldEqual List()
      states.head.get(o3.id).get shouldEqual  SPValue(OperationState.init)
    }


    "run ops, with connected resource variables" in {
      val logic = new OperationRunnerLogic{def log = akka.event.Logging.getLogger(system, this)}

      val r1 = Thing("r1")
      val r2 = Thing("r2")
      val driverR1 = ID.newID
      val driverR2 = ID.newID
      val badID = ID.newID
      println(s"bad id: $badID")


      val startC1 = prop(List(r1, r2), "r1 == 10")
      val startC2 = prop(List(r1, r2), "r2 == 10")


      val o1Upd = o1.copy(conditions = o1.conditions :+ startC1)
      val o2Upd = o2.copy(conditions = o2.conditions :+ startC2)


      val newS = setup.copy(
        opAbilityMap = Map(),
        ops = Set(o1Upd, o2Upd, o3),
        variableMap = Map(r1.id -> driverR1, r2.id -> driverR2)
      )

      logic.addRunner(newS)

      var starting = List[sp.abilityhandler.APIAbilityHandler.StartAbility]()
      val f = (id: ID, param: Map[ID, SPValue]) => starting =
        sp.abilityhandler.APIAbilityHandler.StartAbility(id, param):: starting

      var states = List[SPState]()
      val f2 = (o: SPState, id: ID) => states = o :: states

      logic.setRunnerState(setup.runnerID, initState, f, f2(_, setup.runnerID))

      logic.newResourceState(Map(driverR1 -> 10), f, f2)
      logic.newResourceState(Map(badID -> "hej"), f, f2)
      logic.newResourceState(Map(driverR2 -> 10), f, f2)
      logic.tickRunner(setup.runnerID, f, f2(_, setup.runnerID))


      states.head.get(o2.id).get shouldEqual  SPValue(OperationState.finished)
      states.head.get(badID) shouldEqual  None
    }

    "prepare ability parameters" in {
      val logic = new OperationRunnerLogic{def log = akka.event.Logging.getLogger(system, this)}

      val r1 = Thing("r1")
      val r2 = Thing("r2")
      val driverR1 = ID.newID
      val driverR2 = ID.newID

      val initS: Map[ID, SPValue] = setup.initialState ++ Map(r1.id->SPValue(10), r2.id -> SPValue(15))

      val newS = setup.copy(
        variableMap = Map(r1.id -> driverR1, r2.id -> driverR2),
        abilityParameters = Map(a1.id -> Set(r2.id)),
        initialState = initS
      )

      logic.addRunner(newS)
      val r = logic.runners(setup.runnerID)

      val res = logic.prepareAbilityParameters(a1.id, r, initS)

      assert(res.contains(driverR2))

    }


    "run ops, one ability with parameters" in {
      val logic = new OperationRunnerLogic{def log = akka.event.Logging.getLogger(system, this)}


      val driverR1 = ID.newID
      val driverR2 = ID.newID


      val newS = setup.copy(
        opAbilityMap = Map(o1.id -> a1.id),
        variableMap = Map(t1.id -> driverR1, t2.id -> driverR2),
        abilityParameters = Map(a1.id -> Set(t1.id))
      )
      logic.addRunner(newS)


      var starting = List[sp.abilityhandler.APIAbilityHandler.StartAbility]()
      val f = (id: ID, param: Map[ID, SPValue]) => starting =
        sp.abilityhandler.APIAbilityHandler.StartAbility(id, param):: starting

      var states = List[SPState]()
      val f2 = (o: SPState, id: ID) => states = o :: states

      logic.setRunnerState(setup.runnerID, initState, f, f2(_, setup.runnerID))

      logic.newAbilityState(a1.id, SPValue("enabled"), f, f2)
      logic.newAbilityState(a1.id, SPValue("finished"), f, f2)

      logic.tickRunner(setup.runnerID, f, f2(_, setup.runnerID))


      println("lasflksndf")
      println(starting)
      //println(states)

      val resParams = Map(driverR1 -> SPValue(1))

      starting shouldEqual List(
        sp.abilityhandler.APIAbilityHandler.StartAbility(a1.id, resParams)
      )
      states.head.get(o3.id).get shouldEqual  SPValue(OperationState.finished)
    }




    "testing messages" in {
      val s = OperationRunnerInfo.apischema
      println(s)
      val t = api.GetRunners
      println(SPValue(t))
    }

  }


}

import sp.domain.logic.{PropositionParser, ActionParser}
trait Parsing {
  def v(name: String, drivername: String) = Thing(name, SPAttributes("drivername" -> drivername))
  def prop(vars: List[IDAble], cond: String, actions: List[String] = List(), kind: String = "pre", group: String = "") = {
    def c(condition: String): Option[Proposition] = {
      PropositionParser(vars).parseStr(condition) match {
        case Right(p) => Some(p)
        case Left(err) => println(s"Parsing failed on condition: $condition: $err"); None
      }
    }

    def a(actions: List[String]): List[Action] = {
      actions.flatMap { action =>
        ActionParser(vars).parseStr(action) match {
          case Right(a) => Some(a)
          case Left(err) => println(s"Parsing failed on action: $action: $err"); None
        }
      }
    }

    val cRes = if (cond.isEmpty) AlwaysTrue else c(cond).get
    val aRes = a(actions)

    val attr = SPAttributes("kind" -> kind) ++ {
      if (group.nonEmpty) SPAttributes("group" -> group) else SPAttributes()}

    Condition(cRes, aRes, attr)
  }

}