package sp.runners

import org.scalatest._
import sp.domain.Logic._
import sp.domain._

/**
  * Created by kristofer on 2017-03-06.
  */
class RunnerLogicTest extends FreeSpec with Matchers{
  import sp.abilityhandler.{APIAbilityHandler => api}


  val v1 = Thing("v1")
  val pre = Condition(EQ(v1.id, 1), List(Action(v1.id, ValueHolder(2))), SPAttributes("kind"->"pre", "group" -> "foo"))
  val pre2 = Condition(EQ(v1.id, 2), List(Action(v1.id, ValueHolder(3))), SPAttributes("kind"->"pre", "group" -> "bar"))
  val post = Condition(EQ(v1.id, 3), List(Action(v1.id, ValueHolder(4))), SPAttributes("kind"->"post", "group" -> "foo"))
  val started = Condition(EQ(v1.id, 2), List(), SPAttributes("kind"->"started"))
  val reset = Condition(AlwaysTrue, List(Action(v1.id, ValueHolder(1))), SPAttributes("kind"->"reset", "group" -> "foo"))

  val o = Operation("op", List(pre, pre2, post, started, reset))
  val o2 = Operation("op2", List(pre, pre2, post, started, reset))
  val o3 = Operation("op3", List(pre, pre2, post, started, reset))
  val state = SPState("s", Map(
    v1.id -> 1,
    o.id -> "init",
    o2.id -> "init",
    o3.id -> "executing"
  ))

  import RunnerLogic._


  "Testing the runner logic" - {

    val t1 =   OperationTransition(Set("init"), Set("pre"), "executing", Some("start"))
    val t2 =   OperationTransition(Set("init"), Set("fail"), "failure", None, false)
    val t3 =   OperationTransition(Set("executing"), Set("post"), "finished")
    val t4 =   OperationTransition(Set("finished"), Set("reset"), "init")

    val t = List(t1, t2, t3, t4)


    "possible transitions" - {
      "finding pre" in {
        val res = possibleTransitions(
          op = o,
          s = state,
          fire = List(),
          controlledTransitions = List(),
          unControlledTransitions = t
        )

        res shouldEqual List(t1, t2)
      }

      "finding post" in {

        val res = possibleTransitions(
          op = o,
          s = state.next(o.id -> SPValue("executing")),
          fire = List(),
          controlledTransitions = List(),
          unControlledTransitions = t
        )

        res shouldEqual List(t3)
      }
      "testing error prints" in {

        println("TESTING ERROR prints")
        val res = possibleTransitions(
          op = o,
          s = SPState("s", Map(v1.id -> 1)),
          fire = List(),
          controlledTransitions = List(),
          unControlledTransitions = t
        )
        val res2 = possibleTransitions(
          op = o,
          s = state.next(o.id -> SPValue("foo")),
          fire = List(),
          controlledTransitions = List(),
          unControlledTransitions = t
        )


      }
    }

    "running ops testing" - {


      val v1 = Thing("v1")
      val pre = Condition(EQ(v1.id, 0), List(Action(v1.id, ValueHolder(10))), SPAttributes("kind"->"pre", "group" -> "foo"))
      val pre2 = Condition(EQ(v1.id, 1), List(Action(v1.id, ValueHolder(2))), SPAttributes("kind"->"pre", "group" -> "foo"))
      val post = Condition(EQ(v1.id, 3), List(), SPAttributes("kind"->"post", "group" -> "foo"))
      val post2 = Condition(EQ(v1.id, 4), List(Action(v1.id, ValueHolder(5))), SPAttributes("kind"->"post", "group" -> "foo"))
      val pre3 = Condition(EQ(v1.id, 2), List(Action(v1.id, ValueHolder(3))), SPAttributes("kind"->"pre", "group" -> "foo"))
      val pre4 = Condition(EQ(v1.id, 3), List(Action(v1.id, ValueHolder(0))), SPAttributes("kind"->"pre", "group" -> "foo"))


      val o = Operation("op", List(pre2, post, post2))
      val o2 = Operation("op2", List(pre3))
      val o3 = Operation("op3", List(pre4))
      val state = SPState("s", Map(
        v1.id -> 1,
        o.id -> "init",
        o2.id -> "init",
        o3.id -> "init"
      ))

      val tm = List(
        OperationTransition(Set("init"), Set("pre"), "executing", Some("start")),
        OperationTransition(Set("executing"), Set("post"), "finished", None, true, true),
      )


      "running one operation" in {
        val res = runOperations(
          ops = List(o, o3),
          s = state,
          fire = List(),
          controlledTransitions = List(),
          unControlledTransitions = tm,
          disabledGroups = Set("bar")
        )

        res.sequence.length shouldEqual 1
        res.sequence.head._1 shouldEqual o
        res.lastState.get(v1.id) shouldEqual Some(SPValue(2))

      }

      "running the operations" in {
        val res = runOperations(
          ops = List(o, o2, o3),
          s = state,
          fire = List(),
          controlledTransitions = List(),
          unControlledTransitions = tm,
          disabledGroups = Set("bar")
        )

        res.sequence.reverse.head._1 shouldEqual o
        res.sequence.length shouldEqual 3
        res.lastState.get(v1.id) shouldEqual Some(SPValue(0))
        res.lastState.get(o.id) shouldEqual Some(SPValue("executing"))
        res.lastState.get(o3.id) shouldEqual Some(SPValue("executing"))


//        res.foreach{kv =>
//          println(kv._1.name)
//          kv._2.state.map(println)
//        }

      }


      "running the operations twice" in {
        val res = runOperations(
          ops = List(o, o2),
          s = state,
          fire = List(),
          controlledTransitions = List(),
          unControlledTransitions = tm,
          disabledGroups = Set("bar")
        )

        println(res.lastState)


        val res2 = runOperations(
          ops = List(o, o2),
          s = res.lastState,
          fire = List(),
          controlledTransitions = List(),
          unControlledTransitions = tm,
          disabledGroups = Set("bar")
        )

        println(res2.lastState)

        res2.sequence.head._1 shouldEqual o
        res2.lastState.get(v1.id) shouldEqual Some(SPValue(3))
        res2.lastState.get(o.id) shouldEqual Some(SPValue("finished"))

        // alternative post
        val res3 = runOperations(
          ops = List(o, o2),
          s = res.lastState.next(v1.id -> SPValue(4)),
          fire = List(),
          controlledTransitions = List(),
          unControlledTransitions = tm,
          disabledGroups = Set("bar")
        )

        println(res3.lastState)

        res3.sequence.head._1 shouldEqual o
        res3.lastState.get(v1.id) shouldEqual Some(SPValue(5))
        res3.lastState.get(o.id) shouldEqual Some(SPValue("finished"))



      }

      "not starting when controllable transition" in {
        val res = runOperations(
          ops = List(o, o3),
          s = state,
          fire = List(),
          controlledTransitions = tm,
          unControlledTransitions = List(),
          disabledGroups = Set("bar")
        )

        res.sequence.length shouldEqual 0
        res.lastState shouldEqual state
      }


      "starting when controllable transition is fired" in {
        val res = runOperations(
          ops = List(o, o3),
          s = state,
          fire = List(FireEvent("start", o.id)),
          controlledTransitions = tm,
          unControlledTransitions = List(),
          disabledGroups = Set("bar")
        )

        res.sequence.length shouldEqual 1
        res.sequence.head._1 shouldEqual o
        res.lastState.get(v1.id) shouldEqual Some(SPValue(2))
      }

      "starting multiple ops when controllable transitions are fired" in {
        val res = runOperations(
          ops = List(o, o2, o3),
          s = state,
          fire = List(FireEvent("start", o.id), FireEvent("start", o2.id)),
          controlledTransitions = tm,
          unControlledTransitions = List(),
          disabledGroups = Set("bar")
        )

        res.sequence.length shouldEqual 2
        res.sequence.head._1 shouldEqual o2
        res.lastState.get(v1.id) shouldEqual Some(SPValue(3))
      }


      "starting uncontrollable but not controllable" in {
        val res = runOperations(
          ops = List(o, o2, o3),
          s = state.next(o3.id -> SPValue("executing")),
          fire = List(),
          controlledTransitions = List(OperationTransition(Set("init"), Set("pre"), "executing", Some("start"))),
          unControlledTransitions = List(OperationTransition(Set("executing"), Set("post"), "finished")),
          disabledGroups = Set("bar")
        )

        res.sequence.length shouldEqual 1
        res.sequence.head._1 shouldEqual o3
        res.lastState shouldEqual state.next(o3.id -> SPValue("finished"))
      }


      "Handle many operations" in {
        val pre = Condition(EQ(v1.id, 0), List(Action(v1.id, ValueHolder(0))), SPAttributes("kind"->"pre", "group" -> "foo"))
        val tm = List(
          OperationTransition(Set("init"), Set("pre"), "executing", Some("start")),
          OperationTransition(Set("executing"), Set("post"), "finished", None, true),
        )
        val xs = (1 to 1000).map { i =>
          Operation(i.toString, List(pre))
        }.toList

        val allS = SPState("s", xs.map(o => o.id -> SPValue("init")).toMap).next(v1.id -> SPValue(0))

        println("starting...")

        val res = runOperations(
          ops = xs,
          s = allS,
          fire = List(),
          controlledTransitions = List(),
          unControlledTransitions = tm,
          disabledGroups = Set()
        )

        println("done...")
        println(res.sequence.length)


      }
    }



  }


  "Evalute OP evaluations" - {
    "filter conditions" - {
      "filter pre" in {
        val res = filterConditions(
          conds = o.conditions,
          kind = Set("pre"),
          disabledGroups = Set()
        )

        res shouldEqual List(pre, pre2)
      }

      "filter pre conditions with group" in {
        val res = filterConditions(
          conds = o.conditions,
          kind = Set("pre"),
          disabledGroups = Set("foo")
        )

        res shouldEqual List(pre2)
      }

      "filter started conditions" in {
        val res = filterConditions(
          conds = o.conditions,
          kind = Set("started"),
          disabledGroups = Set()
        )

        res shouldEqual List(started)
      }

      "filter conditions that do not exists" in {
        val res = filterConditions(
          conds = o.conditions,
          kind = Set("non"),
          disabledGroups = Set()
        )

        res shouldEqual List()
      }
    }

    "eval ops" - {
      "pre with foo and bar" in {
        val res = evaluateOP(
          op = o,
          s = state,
          kind = Set("pre"),
          disabledGroups = Set(),
          alwaysTrueIfNoConditions = true,
          enableAlternatives = false
        )

        assert(!res) // should be false
      }

      "pre with foo" in {
        val res = evaluateOP(
          op = o,
          s = state,
          kind = Set("pre"),
          disabledGroups = Set("bar"),
          alwaysTrueIfNoConditions = true,
          enableAlternatives = false
        )

        assert(res) // should be true
      }

      "always true when no condition" in {
        val res = evaluateOP(
          op = o,
          s = state,
          kind = Set("noKind"),
          disabledGroups = Set("bar"),
          alwaysTrueIfNoConditions = true,
          enableAlternatives = false
        )

        assert(res)

        val res2 = evaluateOP(
          op = o,
          s = state,
          kind = Set("noKind"),
          disabledGroups = Set("bar"),
          alwaysTrueIfNoConditions = false,
          enableAlternatives = false
        )

        assert(!res2)

      }

      "enabled when alternatives are enabled" in {
        val res = evaluateOP(
          op = o,
          s = state,
          kind = Set("pre"),
          disabledGroups = Set(),
          alwaysTrueIfNoConditions = true,
          enableAlternatives = true
        )

        assert(res)

        val res2 = evaluateOP(
          op = o,
          s = state.next(v1.id -> SPValue(2)),
          kind = Set("pre"),
          disabledGroups = Set(),
          alwaysTrueIfNoConditions = true,
          enableAlternatives = true
        )

        assert(res2)

        val res3 = evaluateOP(
          op = o,
          s = state.next(v1.id -> SPValue(3)),
          kind = Set("pre"),
          disabledGroups = Set(),
          alwaysTrueIfNoConditions = true,
          enableAlternatives = true
        )

        assert(!res3)

      }

      "evaluate with negated guard" in {
        val res = evaluateOP(
          op = o,
          s = state,
          kind = Set("pre"),
          disabledGroups = Set("bar"),
          alwaysTrueIfNoConditions = true,
          enableAlternatives = false,
          negateGuard = true
        )

        assert(!res)

        val res2 = evaluateOP(
          op = o,
          s = state,
          kind = Set("pre"),
          disabledGroups = Set(),
          alwaysTrueIfNoConditions = true,
          enableAlternatives = false,
          negateGuard = true
        )

        assert(res2) // should be true
      }




      "testing various evals" in {
        val res = evaluateOP(
          op = o,
          s = state,
          kind = Set("reset"),
          disabledGroups = Set(),
          alwaysTrueIfNoConditions = true,
          enableAlternatives = true
        )

        assert(res)

        val res2 = evaluateOP(
          op = o,
          s = state.next(v1.id -> SPValue(3)),
          kind = Set("post"),
          disabledGroups = Set(),
          alwaysTrueIfNoConditions = true,
          enableAlternatives = true
        )

        assert(res2)

        val res3 = evaluateOP(
          op = o,
          s = state.next(v1.id -> SPValue(100)),
          kind = Set("started"),
          disabledGroups = Set(),
          alwaysTrueIfNoConditions = true,
          enableAlternatives = true
        )

        assert(!res3)
      }


    }

    "take transition" - {
      "taking pre" in {
        val res = takeTransition(
          op = o,
          s = state,
          kind = Set("pre"),
          nextOPState = "executing",
          disabledGroups = Set("bar"),
          enableAlternatives = false,
          onlyGuard = false
        )

        res shouldEqual state.next(Map(
          v1.id -> SPValue(2),
          o.id -> SPValue("executing"))
        )

        val res2 = takeTransition(
          op = o,
          s = state,
          kind = Set("post"),
          nextOPState = "finished",
          disabledGroups = Set(),
          enableAlternatives = false,
          onlyGuard = false
        )

        res2 shouldEqual state.next(Map(
          v1.id -> SPValue(4),
          o.id -> SPValue("finished"))
        )
      }

      "taking pre with 2 conds" in {
        val res = takeTransition(
          op = o,
          s = state,
          kind = Set("pre"),
          nextOPState = "executing",
          disabledGroups = Set(),
          enableAlternatives = false,
          onlyGuard = false
        )

        res shouldEqual state.next(Map(
          v1.id -> SPValue(3),
          o.id -> SPValue("executing"))
        )
      }


      "taking when alternatives enabled" in {
        val res = takeTransition(
          op = o,
          s = state,
          kind = Set("pre"),
          nextOPState = "executing",
          disabledGroups = Set(),
          enableAlternatives = true,
          onlyGuard = false
        )

        res shouldEqual state.next(Map(
          v1.id -> SPValue(2),
          o.id -> SPValue("executing"))
        )

        val res2 = takeTransition(
          op = o,
          s = state.next(v1.id -> SPValue(2)),
          kind = Set("pre"),
          nextOPState = "executing",
          disabledGroups = Set(),
          enableAlternatives = true,
          onlyGuard = false
        )

        res2 shouldEqual state.next(Map(
          v1.id -> SPValue(3),
          o.id -> SPValue("executing"))
        )

        val res3 = takeTransition(
          op = o,
          s = state.next(v1.id -> SPValue(10)),
          kind = Set("pre"),
          nextOPState = "executing",
          disabledGroups = Set(),
          enableAlternatives = true,
          onlyGuard = false
        )

        res3 shouldEqual state.next(Map(
          v1.id -> SPValue(10), // no change
          o.id -> SPValue("executing"))
        )



        val pre = Condition(EQ(v1.id, 1), List(Action(v1.id, ValueHolder(10))), SPAttributes("kind"->"pre", "group" -> "foo"))
        val pre2 = Condition(EQ(o2.id, "init"), List(Action(o2.id, ValueHolder("kalle"))), SPAttributes("kind"->"pre", "group" -> "foo"))
        val pre3 = Condition(EQ(o3.id, "no"), List(Action(o3.id, ValueHolder("nono"))), SPAttributes("kind"->"pre", "group" -> "foo"))
        val oTest = Operation("oTest", List(pre, pre2))
        val res4 = takeTransition(
          op = oTest,
          s = state,
          kind = Set("pre"),
          nextOPState = "executing",
          disabledGroups = Set(),
          enableAlternatives = true,
          onlyGuard = false
        )

        res4 shouldEqual state.next(Map(
          v1.id -> SPValue(10),
          oTest.id -> SPValue("executing"),
          o2.id -> SPValue("kalle"))
        )

      }

      "taking transition with only guard" in {
        val res = takeTransition(
          op = o,
          s = state,
          kind = Set("pre"),
          nextOPState = "executing",
          disabledGroups = Set("bar"),
          enableAlternatives = false,
          onlyGuard = true
        )

        res shouldEqual state.next(Map(
          v1.id -> SPValue(1),
          o.id -> SPValue("executing"))
        )

        val res2 = takeTransition(
          op = o,
          s = state,
          kind = Set("pre"),
          nextOPState = "executing",
          disabledGroups = Set("bar"),
          enableAlternatives = false,
          onlyGuard = false
        )

        res2 shouldEqual state.next(Map(
          v1.id -> SPValue(2),
          o.id -> SPValue("executing"))
        )
      }
    }

  }


}