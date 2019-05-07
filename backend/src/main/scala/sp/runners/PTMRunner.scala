package sp.runners

import sp.domain._
import sp.domain.Logic._
import akka.actor.{Actor, Props}
import sp.runners.PTM_Models.StatePredicate


case class PTMRunnerSetState(state: Option[SPState] = None,
                          ops: Option[List[PTM_Models.PTMOperation]] = None,
                          abs: Option[List[PTM_Models.PTMOperation]] = None,
                          opsQ: Option[List[ID]] = None,
                          absQ: Option[List[ID]] = None,
                          model: Option[List[IDAble]] = None,
                          pause: Option[Boolean] = None,
                          step: Option[Boolean] = Some(false),
                          forceState: Option[Map[ID, SPValue]] = None
                         )

case class PTMRunnerState(state: SPState,
                          ops: List[PTM_Models.PTMOperation],
                          abs: List[PTM_Models.PTMOperation],
                          opsQ: List[ID],
                          absQ: List[ID],
                          model: List[IDAble],
                          pause: Boolean = true,
                          forceState: Map[ID, SPValue],
                          predicates: List[StatePredicate]
                      )
object PTMRunnerState {
  def empty = PTMRunnerState(SPState("empty", Map()), List(), List(), List(), List(), List(), true, Map(), List())
}



object PTMRunnerActor {
  def props(init: PTMRunnerSetState) = Props(classOf[PTMRunnerActor], init)
}

class PTMRunnerActor(initialRunnerState: PTMRunnerSetState) extends Actor {
  import PTM_Models._

  var step: Option[Boolean] = Some(false)
  var internal = PTMRunnerState.empty
  setInternalState(initialRunnerState)

  override def receive = {

    case s: SPState =>  // Only allowed to be called via the flow!

      val incomingState = updState(internal.state.next(s.state), internal.predicates, internal.forceState)

      val planningOps = if (! internal.pause)
        runOps(incomingState, internal.ops, internal.opsQ, internal.predicates, internal.forceState, true)
      else
        runPause(incomingState, internal.ops, internal.opsQ)
      val opsPredicates = evaluatePredicates(planningOps.updS, internal.ops)

      val newGoals = makeGoal(opsPredicates)

      if(newGoals != AND(List())) {
        println("new goal: " + prettyPrint(internal.model)(newGoals))

        // Send away new planning for abs and ops with fire and forget. Also send out internal.predicates

        // *** planning in sync for testing
        val aPlanWithBFS = findMeAPlanNuxmv(internal.abs, planningOps.updS, newGoals, internal.model)
        //findMeAPlan(internal.abs, planningOps.updS, newGoals, internal.forceState)
        val abNames = aPlanWithBFS.flatMap { id =>
          internal.abs.flatMap(a => a.controlled.find(_.id == id).map(_.name))
        }
        println("resulting plan: " + abNames)
        internal = internal.copy(absQ = aPlanWithBFS)
        // ***
      }


      val abilities = if(step.contains(false))
        runOps(planningOps.updS, internal.abs, List(), internal.predicates, internal.forceState)
      else if(! internal.pause)
        runOps(planningOps.updS, internal.abs, internal.absQ, internal.predicates, internal.forceState)
      else
        runPause(planningOps.updS, internal.abs, internal.absQ)
      val absPredicates = evaluatePredicates(abilities.updS, internal.abs)

      internal = internal.copy(
        state = abilities.updS,
        opsQ = planningOps.updQ,
        absQ = abilities.updQ
      )

      step = step.map(_ => false)

      val fired = planningOps.fired ++ abilities.fired // We should send this to someone nice
      if (fired.nonEmpty) println(s"the following transition fired: ${fired.map(_.name)}")
      // Maybe also send out if a que was changed?
      // Send out the state internal.predicates or maybe we should include them in the state?

      sender() ! internal.state


    case x: PTMRunnerSetState =>
      setInternalState(x)
      sender() ! true

    case "GetTheData" => sender() ! internal

  }

  def setInternalState(set: PTMRunnerSetState) = {
    internal = PTMRunnerState(
      state = if (set.state.nonEmpty) set.state.get else internal.state,
      ops = if (set.ops.nonEmpty) set.ops.get else internal.ops,
      abs = if (set.abs.nonEmpty) set.abs.get else internal.abs,
      opsQ = if (set.opsQ.nonEmpty) set.opsQ.get else internal.opsQ,
      absQ = if (set.absQ.nonEmpty) set.absQ.get else internal.absQ,
      model = if (set.model.nonEmpty) set.model.get else internal.model,
      pause = if (set.pause.nonEmpty) set.pause.get else internal.pause,
      forceState = if (set.forceState.nonEmpty) set.forceState.get else internal.forceState,
      predicates = List()
    )
    step = set.step
    internal = internal.copy(predicates = (internal.ops ++ internal.abs).flatMap(_.predicates))
    internal = internal.copy(state = updState(internal.state, internal.predicates, internal.forceState))
  }

}






object PTM_Models extends sp.modelSupport.ExportNuXmvFile2 {
  case class StatePredicate(name: String, predicate: Proposition, id: ID = ID.newID)
  case class PTMTransition(condition: Condition, name: String = "", id: ID = ID.newID)

  case class PTMOperation(predicates: List[StatePredicate],
    controlled: List[PTMTransition],
    unControlled: List[PTMTransition],
    effects: List[PTMTransition],
    o: Operation = Operation("noName")
  )
  case class ControlQue(xs: List[ID])
  case class OneStep(updS: SPState, updQ: List[ID], fired: List[PTMTransition])


  def runOps(s: SPState,
    ops: List[PTMOperation],
    q: List[ID],
    predicates: List[StatePredicate] = List(),
    forceState: Map[ID, SPValue] = Map(),
    withoutQ: Boolean = false) = {
    val cs = ops.flatMap(_.controlled)
    val us = ops.flatMap(_.unControlled)
    val os = if(withoutQ) {
      val res = runOneStepSeqWithoutQ(s, cs, us, predicates, forceState)
      OneStep(res._1, q, res._2)
    } else {
      val res = runOneStepSeq(s, cs, us, ControlQue(q), predicates, forceState)
      OneStep(res._1, res._2.xs, res._3)
    }
    os
  }

  def runPause(s: SPState, ops: List[PTMOperation], q: List[ID]): OneStep = {
    OneStep(s, q, List())
  }

  def evaluatePredicates(s: SPState, xs: List[PTMOperation]): Map[PTMOperation, List[StatePredicate]] = {
    xs.map(o =>  o -> o.predicates.filter(_.predicate.eval(s))).toMap
  }



  // Runs all transition in sequence
  def runOneStepSeq(state: SPState,
                    controlled: List[PTMTransition],
                    unControlled: List[PTMTransition],
                    que: ControlQue,
                    predicates: List[StatePredicate] = List(),
                    forceState: Map[ID, SPValue] = Map()): (SPState, ControlQue, List[PTMTransition]) = {

    // Some initial test that we can remove or do something with later
    que.xs.foreach{t =>
      if (!controlled.exists(c => c.id == t) ) println(s"transition $t in the que, does not exist in the controlled list")
    }

    val cT = for {
      toStart <- que.xs.headOption
      transition <- controlled.find(_.id == toStart)
      if transition.condition.eval(state)
    } yield transition

    val tryControlledTransition = cT.map(x => updState(x.condition.next(state), predicates, forceState))
    val updQue = if (tryControlledTransition.isDefined) ControlQue(que.xs.tail) else que // remove head in que if transition taken

    val stateAfterControlled = tryControlledTransition.getOrElse(state)
    val stateAfterUncontrolled = unControlled.foldLeft(stateAfterControlled, cT.toList){case (sl, t) =>
      if (t.condition.eval(sl._1)){
        (updState(t.condition.next(sl._1), predicates, forceState), sl._2 :+ t)
      } else
        sl
    }

    (stateAfterUncontrolled._1, updQue, stateAfterUncontrolled._2)
  }

  def runOneStepSeqWithoutQ(state: SPState,
                    controlled: List[PTMTransition],
                    unControlled: List[PTMTransition],
                    predicates: List[StatePredicate] = List(),
                    forceState: Map[ID, SPValue] = Map()): (SPState, List[PTMTransition]) = {


    val stateAfterUncontrolled = (controlled ++ unControlled).foldLeft(state, List[PTMTransition]()){case (sl, t) =>
      if (t.condition.eval(sl._1)){
        (updState(t.condition.next(sl._1), predicates, forceState), sl._2 :+ t)
      } else
        sl
    }

    (stateAfterUncontrolled._1, stateAfterUncontrolled._2)
  }


  // returns only changed. Maybe better
//  def next(c: Condition, s: SPState): Map[ID, SPValue] = {
//    c.action.map(a => a.id -> a.nextValue(s)).toMap
//  }

  def updState(s: SPState, predicates: List[StatePredicate], force: Map[ID, SPValue]): SPState = {
    val forceS = s.next(force)
    val updP = predicates.map(p => p.id -> SPValue(p.predicate.eval(forceS))).toMap
    forceS.next(updP)
  }

  def makePlanningOP(name: String, start: Condition, goal: Condition, op: Option[Operation] = None): (PTMOperation, Thing) = {
    val variable = Thing(name, SPAttributes("domain" -> List("i", "e", "f"), "initialState" -> "i"))
    val init = StatePredicate("i", EQ(SVIDEval(variable.id), ValueHolder("i")))
    val execute = StatePredicate("e", EQ(SVIDEval(variable.id), ValueHolder("e")))
    val pre = PTMTransition(
      start.copy(
        guard = AND(List(init.predicate, start.guard)),
        action = Action(variable.id, ValueHolder("e")) +: start.action,
        attributes = start.attributes + ("kind" -> "pre")
      ),
      "pre"
    )
    val post = PTMTransition(
      goal.copy(
        guard = AND(List(execute.predicate, goal.guard)),
        action = Action(variable.id, ValueHolder("f")) +: goal.action,
        attributes = goal.attributes + ("kind" -> "post")
      ),
      "post"
    )
    (PTMOperation(List(init, execute), List(pre), List(post), List(), op.getOrElse(Operation("OP_"+name))), variable)
  }


  def makeGoal(predicateMap: Map[PTMOperation, List[StatePredicate]]): Proposition = {
    val runningOps = predicateMap.filter(o => o._2.exists(x => x.name == "e"))
    val allPosts = runningOps.flatMap(o => o._1.unControlled.filter(_.name == "post").map(_.condition.guard)).toList
    AND(allPosts)
  }

  // How to send in the transitions? Should we convert to operations or update the code in computePlan?
  def planAbilities(abilities: List[PTMOperation], model: List[IDAble], state: SPState, goal: Proposition) = {

    // a temporary dummy planner
    //val enabledOps =

//    val absAsOps = abilities.map{a =>
//
//    }
//
//
//    val (plan, ntrans, stdout, stderr) = computePlan(ids, state.state, 50, goal, "", "/tmp/runner.smv")
//    println(plan)
//    println(ntrans)
//    println(stdout)
//    println(stderr)
  }

  def plansToLTL(plans: List[String]): String = {
    val s = plans.map(p => s"(F ($p))").mkString("&")
    s"! ($s)"
  }

  def findMeAPlanNuxmv(ops: List[PTMOperation], state: SPState, goal: Proposition, model: List[IDAble]): List[ID] = {
    val goalStr = SPpropTonuXmvSyntax(goal,
      model.collect{case v:Thing=>v}.map(v => v.copy(name = "v_"+v.name.replaceAll("\\.", "_"))),         /// TODO: terrible
      model.collect{case o:Operation=>o}.map(o => o.copy(name = "o_"+o.name.replaceAll("\\.", "_"))))

    val planSpec = s"! F ( $goalStr )"
    val (plan, ntrans, stdout, stderr) = computePlan(model, state.state, 50, AND(List()), planSpec, "/tmp/ptmrunner.smv")

    plan.flatMap { name =>
      ops.find(_.o.name == name).flatMap { o => o.controlled.headOption.map(_.id) }
    }
  }

  // DFS or BFS.
  def findMeAPlan(ops: List[PTMOperation],
                  state: SPState,
                  goal: Proposition,
                  forceState: Map[ID, SPValue] = Map(),
                  maxPath: Int = 30): List[ID] = {
    val controlled = ops.flatMap(_.controlled)
    val unControlled = ops.flatMap(_.unControlled)
    val effects = ops.flatMap(_.effects)
    val ps = ops.flatMap(_.predicates)

    case class PimpedState(state: SPState, path: List[ID])
    var stack = List[PimpedState](PimpedState(state, List()))
    var foundGoal: Option[PimpedState] = None
    var visited = Set[SPState]()

    while (stack.nonEmpty && foundGoal.isEmpty) {
      val s = stack.head
      stack = stack.tail
      if (!visited.contains(s.state)){  // no need to loop in the state!
        visited += s.state
        if (goal.eval(s.state)) foundGoal = Some(s)

        val enabled = controlled.filter(t => t.condition.eval(s.state))
        println("ENABLED: " + enabled)
        val nextStates = enabled.flatMap{t =>
          val next = runOneStepSeq(s.state, controlled, unControlled ++ effects, ControlQue(List(t.id)), ps, forceState)
          val path = t.id :: s.path
          if (path.size > maxPath) None else Some(PimpedState(next._1, path))
        }

        // DFS
        //stack = nextStates ++ stack

        // BFS
        stack = stack ++ nextStates

      }
    }

    foundGoal.map(_.path.reverse).getOrElse(List())
  }



  //  /**
  //    * This runs one controlled transition and multiple uncontrolled. Currently runs all based on
  //    * the given state.
  //    * @param state The input state
  //    * @param controlled A list of controlled transition that only starts based on the que
  //    * @param unControlled A list of uncontrolled transitions that starts when they are enabled
  //    * @param que A list of transition ids defining the starting order
  //    * @return A new state, que and list of fired transitions (just nu namn, sedan id)
  //    *         (and returns the same if they where not updated)
  //    */
  //  def runOneStep(state: SPState,
  //                 controlled: List[PTMTransition],
  //                 unControlled: List[PTMTransition],
  //                 que: ControlQue): (SPState, ControlQue, List[PTMTransition]) = {
  //
  //    // Some initial test that we can remove or do something with later
  //    que.xs.foreach{t =>
  //      if (!controlled.exists(c => c.id == t) ) println(s"transition $t in the que, does not exist in the controlled list")
  //    }
  //
  //    val cT = for {
  //      toStart <- que.xs.headOption
  //      transition <- controlled.find(_.id == toStart)
  //      if transition.condition.eval(state)
  //    } yield transition
  //
  //    val tryControlledTransition = cT.map(t => next(t.condition, state)) // take step if controlled transition is enabled
  //    val updQue = if (tryControlledTransition.isDefined) ControlQue(que.xs.tail) else que // remove head in que if transition taken
  //
  //    val stateAfterUncontrolled = unControlled.foldLeft((Map[ID, SPValue](), cT.toList)){case (sl, t) =>
  //      if (t.condition.eval(state)){
  //        val newS = next(t.condition, state)
  //        // check so that we do not overwrite when newS ++ sl._1
  //        (newS ++ sl._1, sl._2 :+ t)
  //      } else
  //        sl
  //    }
  //
  //    // Maybe we should return only changed values?
  //    val newS = stateAfterUncontrolled._1 ++ tryControlledTransition.getOrElse(Map())
  //
  //    (state.next(newS), updQue, stateAfterUncontrolled._2)
  //  }


}
