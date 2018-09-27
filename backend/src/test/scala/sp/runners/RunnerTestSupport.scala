package sp.runners

import sp.domain._
import Logic._

// Move to some file

trait SimpleSequence extends LocationsAndConditionKinds {
  val v1 = Thing("v1")
  val pre1 = Condition(EQ(v1.id, 0), List(), SPAttributes("kind"->pre, "group" -> "foo"))
  val post1 = Condition(AlwaysTrue, List(Action(v1.id, ValueHolder(1))), SPAttributes("kind"->post, "group" -> "foo"))
  val pre2 = Condition(EQ(v1.id, 1), List(), SPAttributes("kind"->pre, "group" -> "foo"))
  val post2 = Condition(AlwaysTrue, List(Action(v1.id, ValueHolder(2))), SPAttributes("kind"->post, "group" -> "foo"))
  val pre3 = Condition(EQ(v1.id, 2), List(), SPAttributes("kind"->pre, "group" -> "foo"))
  val post3 = Condition(AlwaysTrue, List(Action(v1.id, ValueHolder(3))), SPAttributes("kind"->post, "group" -> "foo"))


  val o1 = Operation("op1", List(pre1, post1))
  val o2 = Operation("op2", List(pre2, post2))
  val o3 = Operation("op3", List(pre3, post3))
  val ops = List(o1, o2, o3)
  val initialState = SPState("s", Map(
    v1.id -> 0,
    o1.id -> init,
    o2.id -> init,
    o3.id -> init
  ))

}

trait LocationsAndConditionKinds {
  val init = "init"
  val executing = "executing"
  val finished = "finished"

  val pre = "pre"
  val post = "post"
  val reset = "reset"
  val forceReset = "forceReset"

}

trait OperationRunnerTransitionsNoReset extends LocationsAndConditionKinds{
  val transitions = List(
    RunnerLogic.OperationTransition(Set(init), pre, executing),
    RunnerLogic.OperationTransition(Set(executing), post, finished),
  )
}
trait OperationRunnerTransitionsWithAutoReset extends LocationsAndConditionKinds {
  val transitions = List(
    RunnerLogic.OperationTransition(Set(init), pre, executing),
    RunnerLogic.OperationTransition(Set(executing), post, finished),
    RunnerLogic.OperationTransition(Set(finished), reset, init),
    RunnerLogic.OperationTransition(Set(executing), reset, init, Some("reset")),
  )
}

trait OperationRunnerWithStartEvents extends LocationsAndConditionKinds{
  val transitions = List(
    RunnerLogic.OperationTransition(Set(init), pre, executing, Some("start")),
    RunnerLogic.OperationTransition(Set(executing), post, finished),
  )
}