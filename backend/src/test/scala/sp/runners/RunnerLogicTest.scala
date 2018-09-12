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

  val logic = new RunnerLogic {}


  "Testing the runner logic" - {
    val t = List(
        logic.OperationTransition("init", Set("pre"), "executing"),
        logic.OperationTransition("init", Set("fail"), "failure", false),
        logic.OperationTransition("executing", Set("post"), "finished"),
        logic.OperationTransition("finished", Set("reset"), "init")
    )

    "possible transitions" - {
      "finding pre" in {
        val res = logic.possibleTransitions(
          op = o,
          s = state,
          transitions = t
        )

        res shouldEqual List(
          logic.OperationTransition("init", Set("pre"), "executing"),
          logic.OperationTransition("init", Set("fail"), "failure", false))
      }
      "finding post" in {
        val res = logic.possibleTransitions(
          op = o,
          s = state.next(o.id -> SPValue("executing")),
          transitions = t
        )

        res shouldEqual List(
          logic.OperationTransition("executing", Set("post"), "finished"),
        )
      }
      "testing error prints" in {

        println("TESTING ERROR prints")
        logic.possibleTransitions(
          op = o,
          s = SPState("s", Map(v1.id -> 1)),
          transitions = t
        )
        logic.possibleTransitions(
          op = o,
          s = state.next(o.id -> SPValue("foo")),
          transitions = t
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
        logic.OperationTransition("init", Set("pre"), "executing"),
        logic.OperationTransition("executing", Set("post"), "finished", false, true),
      )


      "running one operation" in {
        val res = logic.runOperations(
          ops = List(o, o3),
          s = state,
          aggr = List(),
          transitions = tm,
          disabledGroups = Set("bar")
        )

        res.length shouldEqual 1
        res.head._1 shouldEqual o
        res.head._2.get(v1.id) shouldEqual Some(SPValue(2))

      }

      "running the operations" in {
        val res = logic.runOperations(
          ops = List(o, o2, o3),
          s = state,
          aggr = List(),
          transitions = tm,
          disabledGroups = Set("bar")
        ).reverse

        res.head._1 shouldEqual o
        res.head._2.get(v1.id) shouldEqual Some(SPValue(2))
        res.head._2.get(o.id) shouldEqual Some(SPValue("executing"))
        res.head._2.get(o3.id) shouldEqual Some(SPValue("init"))

        res(2)._2.get(o3.id) shouldEqual Some(SPValue("executing"))

//        res.foreach{kv =>
//          println(kv._1.name)
//          kv._2.state.map(println)
//        }

      }


      "running the operations twice" in {
        val res = logic.runOperations(
          ops = List(o, o2),
          s = state,
          aggr = List(),
          transitions = tm,
          disabledGroups = Set("bar")
        )

        val lastState = res.head._2
        println(lastState)

        val res2 = logic.runOperations(
          ops = List(o, o2),
          s = lastState,
          aggr = List(),
          transitions = tm,
          disabledGroups = Set("bar")
        )

        res2.head._1 shouldEqual o
        res2.head._2.get(v1.id) shouldEqual Some(SPValue(3))
        res2.head._2.get(o.id) shouldEqual Some(SPValue("finished"))

        // alternative post
        val res3 = logic.runOperations(
          ops = List(o, o2),
          s = lastState.next(v1.id -> SPValue(4)),
          aggr = List(),
          transitions = tm,
          disabledGroups = Set("bar")
        )

        res3.head._1 shouldEqual o
        res3.head._2.get(v1.id) shouldEqual Some(SPValue(5))
        res3.head._2.get(o.id) shouldEqual Some(SPValue("finished"))

      }
    }

  }


  "Evalute OP evaluations" - {
    "filter conditions" - {
      "filter pre" in {
        val res = logic.filterConditions(
          conds = o.conditions,
          kind = Set("pre"),
          disabledGroups = Set()
        )

        res shouldEqual List(pre, pre2)
      }

      "filter pre conditions with group" in {
        val res = logic.filterConditions(
          conds = o.conditions,
          kind = Set("pre"),
          disabledGroups = Set("foo")
        )

        res shouldEqual List(pre2)
      }

      "filter started conditions" in {
        val res = logic.filterConditions(
          conds = o.conditions,
          kind = Set("started"),
          disabledGroups = Set()
        )

        res shouldEqual List(started)
      }

      "filter conditions that do not exists" in {
        val res = logic.filterConditions(
          conds = o.conditions,
          kind = Set("non"),
          disabledGroups = Set()
        )

        res shouldEqual List()
      }
    }

    "eval ops" - {
      "pre with foo and bar" in {
        val res = logic.evaluateOP(
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
        val res = logic.evaluateOP(
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
        val res = logic.evaluateOP(
          op = o,
          s = state,
          kind = Set("noKind"),
          disabledGroups = Set("bar"),
          alwaysTrueIfNoConditions = true,
          enableAlternatives = false
        )

        assert(res)

        val res2 = logic.evaluateOP(
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
        val res = logic.evaluateOP(
          op = o,
          s = state,
          kind = Set("pre"),
          disabledGroups = Set(),
          alwaysTrueIfNoConditions = true,
          enableAlternatives = true
        )

        assert(res)

        val res2 = logic.evaluateOP(
          op = o,
          s = state.next(v1.id -> SPValue(2)),
          kind = Set("pre"),
          disabledGroups = Set(),
          alwaysTrueIfNoConditions = true,
          enableAlternatives = true
        )

        assert(res2)

        val res3 = logic.evaluateOP(
          op = o,
          s = state.next(v1.id -> SPValue(3)),
          kind = Set("pre"),
          disabledGroups = Set(),
          alwaysTrueIfNoConditions = true,
          enableAlternatives = true
        )

        assert(!res3)

      }




      "testing various evals" in {
        val res = logic.evaluateOP(
          op = o,
          s = state,
          kind = Set("reset"),
          disabledGroups = Set(),
          alwaysTrueIfNoConditions = true,
          enableAlternatives = true
        )

        assert(res)

        val res2 = logic.evaluateOP(
          op = o,
          s = state.next(v1.id -> SPValue(3)),
          kind = Set("post"),
          disabledGroups = Set(),
          alwaysTrueIfNoConditions = true,
          enableAlternatives = true
        )

        assert(res2)

        val res3 = logic.evaluateOP(
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
        val res = logic.takeTransition(
          op = o,
          s = state,
          kind = Set("pre"),
          nextOPState = "executing",
          disabledGroups = Set("bar"),
          enableAlternatives = false
        )

        res shouldEqual state.next(Map(
          v1.id -> SPValue(2),
          o.id -> SPValue("executing"))
        )

        val res2 = logic.takeTransition(
          op = o,
          s = state,
          kind = Set("post"),
          nextOPState = "finished",
          disabledGroups = Set(),
          enableAlternatives = false
        )

        res2 shouldEqual state.next(Map(
          v1.id -> SPValue(4),
          o.id -> SPValue("finished"))
        )
      }

      "taking pre with 2 conds" in {
        val res = logic.takeTransition(
          op = o,
          s = state,
          kind = Set("pre"),
          nextOPState = "executing",
          disabledGroups = Set(),
          enableAlternatives = false
        )

        res shouldEqual state.next(Map(
          v1.id -> SPValue(3),
          o.id -> SPValue("executing"))
        )
      }


      "taking when alternatives enabled" in {
        val res = logic.takeTransition(
          op = o,
          s = state,
          kind = Set("pre"),
          nextOPState = "executing",
          disabledGroups = Set(),
          enableAlternatives = true
        )

        res shouldEqual state.next(Map(
          v1.id -> SPValue(2),
          o.id -> SPValue("executing"))
        )

        val res2 = logic.takeTransition(
          op = o,
          s = state.next(v1.id -> SPValue(2)),
          kind = Set("pre"),
          nextOPState = "executing",
          disabledGroups = Set(),
          enableAlternatives = true
        )

        res2 shouldEqual state.next(Map(
          v1.id -> SPValue(3),
          o.id -> SPValue("executing"))
        )

        val res3 = logic.takeTransition(
          op = o,
          s = state.next(v1.id -> SPValue(10)),
          kind = Set("pre"),
          nextOPState = "executing",
          disabledGroups = Set(),
          enableAlternatives = true
        )

        res3 shouldEqual state.next(Map(
          v1.id -> SPValue(10), // no change
          o.id -> SPValue("executing"))
        )



        val pre = Condition(EQ(v1.id, 1), List(Action(v1.id, ValueHolder(10))), SPAttributes("kind"->"pre", "group" -> "foo"))
        val pre2 = Condition(EQ(o2.id, "init"), List(Action(o2.id, ValueHolder("kalle"))), SPAttributes("kind"->"pre", "group" -> "foo"))
        val pre3 = Condition(EQ(o3.id, "no"), List(Action(o3.id, ValueHolder("nono"))), SPAttributes("kind"->"pre", "group" -> "foo"))
        val oTest = Operation("oTest", List(pre, pre2))
        val res4 = logic.takeTransition(
          op = oTest,
          s = state,
          kind = Set("pre"),
          nextOPState = "executing",
          disabledGroups = Set(),
          enableAlternatives = true
        )

        res4 shouldEqual state.next(Map(
          v1.id -> SPValue(10),
          oTest.id -> SPValue("executing"),
          o2.id -> SPValue("kalle"))
        )

      }
    }

  }


}
