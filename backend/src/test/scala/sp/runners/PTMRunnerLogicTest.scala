package sp.runners

import org.scalatest._
import sp.domain._
import sp.domain.logic._
import sp.domain.Logic._
import sp.runners.PTM_Models._

class PTMRunnerLogicTest extends FreeSpec with Matchers {

  "testing PTM logic runner" - {
    val m = new TestModel {}
    import m._
//    "uncontrolled ordering testing" in {
//
//      val initState = SPState(state = Map(v1.id -> f, v2.id -> f, v3.id -> f))
//
//      val res = runOneStepSeq(
//        state = initState,
//        controlled = List(),
//        unControlled = List(t1, t2, t3),
//        ControlQue(List())
//      )
//
//      val res2 = runOneStepSeq(
//        state = initState,
//        controlled = List(),
//        unControlled = List(t2, t1, t3),
//        ControlQue(List())
//      )
//
//      val res3 = runOneStepSeq(
//        state = initState,
//        controlled = List(),
//        unControlled = List(t3, t1, t2),
//        ControlQue(List())
//      )
//
//      res shouldEqual res2
//      res2 shouldEqual res3
//    }

    "uncontrolled ordering testing when in seq" in {

      val initState = SPState(state = Map(v1.id -> f, v2.id -> f, v3.id -> f))

      val res = runOneStepSeq(
        state = initState,
        controlled = List(),
        unControlled = List(t1, t2, t3),
        ControlQue(List())
      )

      val res2 = runOneStepSeq(
        state = initState,
        controlled = List(),
        unControlled = List(t2, t1, t3),
        ControlQue(List())
      )

      val res3 = runOneStepSeq(
        state = initState,
        controlled = List(),
        unControlled = List(t3, t1, t2),
        ControlQue(List())
      )

      assert(res._3 == List(t2))
      assert(res2._3 == List(t2, t1, t3))
      assert(res3._3 == List(t2) )

    }

    "test with controlled" in {
      val initState = SPState(state = Map(v1.id -> f, v2.id -> f, v3.id -> f))

      val res = runOneStepSeq(
        state = initState,
        controlled = List(t2),
        unControlled = List(t1, t3),
        ControlQue(List(t2.id))
      )

      assert(res._2.xs.isEmpty && res._3.nonEmpty)

      val res2 = runOneStepSeq(
        state = initState,
        controlled = List(t1),
        unControlled = List(t1, t3),
        ControlQue(List(t2.id))
      )

      assert(res2._2.xs.nonEmpty && res2._3.isEmpty)

    }

    "test with controlled, multiple" in {
      val initState = SPState(state = Map(v1.id -> f, v2.id -> f, v3.id -> f))

      val res = runOneStepSeq(
        state = initState,
        controlled = trans,
        unControlled = List(),
        ControlQue(List(t2.id, t1.id, t3.id))
      )

      val res2 = runOneStepSeq(res._1, trans, List(), res._2)
      val res3 = runOneStepSeq(res2._1, trans, List(), res2._2)

      //println(res3)
      assert(res3._2.xs.isEmpty && res3._3.nonEmpty)

      val ault = runOneStepSeq(
        state = initState,
        controlled = trans,
        unControlled = List(),
        ControlQue(List(t2.id, t3.id, t1.id))
      )

      val ault2 = runOneStepSeq(ault._1, trans, List(), ault._2)
      val ault3 = runOneStepSeq(ault2._1, trans, List(), ault2._2)

      //println(ault3)
      assert(ault3._2.xs.nonEmpty && ault3._3.isEmpty)

    }


    "test with controlled, and uncontrolled multiple" in {
      val initState = SPState(state = Map(v1.id -> f, v2.id -> f, v3.id -> f))

      var cs = List(t1, t3)
      var us = List(t2)
      val res = runOneStepSeq(
        state = initState,
        controlled = cs,
        unControlled = us,
        ControlQue(List(t1.id, t3.id))
      )

      val res2 = runOneStepSeq(res._1, cs, us, res._2)
      val res3 = runOneStepSeq(res2._1, cs, us, res2._2)

      //println(res3)
      assert(res3._2.xs.isEmpty && res3._3.nonEmpty)


      cs = List(t3)
      us = List(t1, t2)
      val ault = runOneStepSeq(
        state = initState,
        controlled = cs,
        unControlled = us,
        ControlQue(List(t3.id))
      )

      val ault2 = runOneStepSeq(ault._1, cs, us, ault._2)
      val ault3 = runOneStepSeq(ault2._1, cs, us, ault2._2)

      println(ault)
      println(ault2)
      println(ault3)
      assert(res3._2.xs.isEmpty && res3._3.nonEmpty)

    }

    "test with controlled, and uncontrolled multiple when in run in seq" in {
      val initState = SPState(state = Map(v1.id -> f, v2.id -> f, v3.id -> f))

      var cs = List(t1, t3)
      var us = List(t2)
      val res = runOneStepSeq(
        state = initState,
        controlled = cs,
        unControlled = us,
        ControlQue(List(t1.id, t3.id))
      )

      val res2 = runOneStepSeq(res._1, cs, us, res._2)
      val res3 = runOneStepSeq(res2._1, cs, us, res2._2)

      //println(res3)
      assert(res3._2.xs.isEmpty && res3._3.nonEmpty)


      cs = List(t3)
      us = List(t1, t2)
      val ault = runOneStepSeq(
        state = initState,
        controlled = cs,
        unControlled = us,
        ControlQue(List(t3.id))
      )

      val ault2 = runOneStepSeq(ault._1, cs, us, ault._2)
      val ault3 = runOneStepSeq(ault2._1, cs, us, ault2._2)

      println(ault)
      println(ault2)
      println(ault3)
      assert(res3._2.xs.isEmpty && res3._3.nonEmpty)

    }


    "Force table is forcing" in {
      val initState = SPState(state = Map(v1.id -> f, v2.id -> f, v3.id -> f))
      val force =  Map(v1.id -> f, v2.id -> f, v3.id -> f)

      var cs = List(t1, t3)
      var us = List(t2)
      val res = runOneStepSeq(
        state = initState,
        controlled = cs,
        unControlled = us,
        ControlQue(List(t1.id, t3.id)),
        List(),
        force
      )

      val res2 = runOneStepSeq(res._1, cs, us, res._2, List(), force)
      val res3 = runOneStepSeq(res2._1, cs, us, res2._2, List(), force)

      //println(res3)
      assert(res3._1 == initState && res2._1 == initState)



    }
    "Force table is forcing, part 2" in {
      val initState = SPState(state = Map(v1.id -> f, v2.id -> f, v3.id -> f))
      val force =  Map(v3.id -> f)

      var cs = List(t1, t3)
      var us = List(t2)
      val res = runOneStepSeq(
        state = initState,
        controlled = cs,
        unControlled = us,
        ControlQue(List(t1.id, t3.id)),
        List(),
        force
      )

      val res2 = runOneStepSeq(res._1, cs, us, res._2, List(), force)
      val res3 = runOneStepSeq(res2._1, cs, us, res2._2, List(), force)

      //println(res3)
      assert(res3._1 == initState.next(v1.id -> t))

    }

    "Test the predicates" in {
      val initState = SPState(state = Map(v1.id -> f, v2.id -> f, v3.id -> f))
      val force =  Map(v3.id -> f)
      val p = StatePredicate("test", parseGuard("v1", ids))
      val p2 = StatePredicate("test_false", parseGuard("v3", ids))

      var cs = List(t1, t3)
      var us = List(t2)
      val res = runOneStepSeq(
        state = initState,
        controlled = cs,
        unControlled = us,
        ControlQue(List(t1.id, t3.id)),
        List(p, p2),
        force
      )

      val res2 = runOneStepSeq(res._1, cs, us, res._2, List(), force)
      val res3 = runOneStepSeq(res2._1, cs, us, res2._2, List(), force)

      println(s"p: $p")
      println(s"p2: $p2")
      println(res3)
      assert(res3._1.get(p.id).contains(t) && res3._1.get(p2.id).contains(f))

    }

    "test runnerlogic with operations" in {
      val initState = SPState(state = Map(v1.id -> f, v2.id -> f, v3.id -> f, v4.id -> f))
      var res = runOps(initState, List(o1, o2), List(preO1.id, preO2.id), List())
      println(s"one step: ${res.fired.map(_.name)}")
      res = runOps(res.updS, List(o1, o2), res.updQ, List())
      println(s"one step: ${res.fired.map(_.name)}")
      res = runOps(res.updS, List(o1, o2), res.updQ, List())
      println(s"one step: ${res.fired.map(_.name)}")
      assert(res.updQ.isEmpty)

    }
  }

}

trait TestModel {
  val v1 = Thing("v1")
  val v2 = Thing("v2")
  val v3 = Thing("v3")
  val v4 = Thing("v4")
  val t = SPValue(true)
  val f = SPValue(false)

  val ids = List(v1, v2, v3, v4)

  val t1 = PTMTransition(
    condition = c(ids, "v1 && !v2", "v3 := true"),
    name = "t1"
  )

  val t2 = PTMTransition(
    condition = c(ids, "!v1", "v1 := true"),
    name = "t2"
  )
  val t3 = PTMTransition(
    condition = c(ids, "v3 == true", "v2 := true"),
    name = "t3"
  )

  val preO1 = PTMTransition(
    condition = c(ids, "!v1", "v1 := true"),
    name = "preO1"
  )
  val postO1 = PTMTransition(
    condition = c(ids, "v1 && !v2", "v2 := true"),
    name = "postO1"
  )


  val preO2 = PTMTransition(
    condition = c(ids, "v2", "v3 := true"),
    name = "preO2"
  )
  val postO2 = PTMTransition(
    condition = c(ids, "v3 && !v4", "v4 := true"),
    name = "postO2"
  )

  val trans = List(t1, t2, t3)

  val o1 = PTMOperation(
    predicates = List(),
    controlled = List(preO1),
    unControlled = List(postO1),
    effects = List()
  )
  val o2 = PTMOperation(
    predicates = List(),
    controlled = List(preO2),
    unControlled = List(postO2),
    effects = List()
  )


  def c(ids: List[IDAble], guard: String, actions: String*) = {
    val g = parseGuard(guard, ids)
    val a = actions.map(x => parseAction(x, ids)).toList
    Condition(g, a)
  }
  def parseGuard(x: String, ids: List[IDAble]): Proposition = {
    val res = PropositionParser(ids).parseStr(x)
    if (res.isLeft) println(res)
    res.toOption.get
  }
  def parseAction(x: String, ids: List[IDAble]): Action = {
    val res = ActionParser(ids).parseStr(x)
    if (res.isLeft) println(res)
    res.toOption.get
  }



}

trait ConditionParseSupport {
  def c(ids: List[IDAble], guard: String, actions: String*) = {
    val g = parseGuard(guard, ids)
    val a = actions.map(x => parseAction(x, ids)).toList
    Condition(g, a)
  }
  def parseGuard(x: String, ids: List[IDAble]): Proposition = {
    val res = PropositionParser(ids).parseStr(x)
    if (res.isLeft) println(res)
    res.toOption.get
  }
  def parseAction(x: String, ids: List[IDAble]): Action = {
    val res = ActionParser(ids).parseStr(x)
    if (res.isLeft) println(res)
    res.toOption.get
  }
}



