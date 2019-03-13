package sp.runners

import sp.domain._
import Logic._

import scala.annotation.tailrec



object RunnerLogic extends sp.modelSupport.ExportNuXmvFile2 {

  /**
    * 180914: We need to decide if each operation should be defined with its own transitions
    * and events. What we want is to tell per operation which has an uncontrollable start
    * and which one that should run in auto (uncontrollable). But it may be really hard to
    * troubleshoot the operation and the runner when something happens.
    *
    * Currently the transition system is defined per runner instance and each
    * operation will have the same.
    *
    * 180915: After discussion, we stick with one general transition system. To be able to
    * let some operation start uncontrollable and some controllable, we can use the firing
    * mechanism to constantly enable those events.
    */



  //type State = Map[ID, SPValue]

  /**
    * This class defines a transition from a state to another in the general transition
    * system used by the runner.
    * @param states The source states (in what states the transition is possible)
    * @param conditionKind The conditions associated with the transiton, e.g. pre, post, ...
    * @param nextState The Sink state, where the operation will be in after the transition
    * @param alwaysTrueIfNoConditions If an operation do not have any associated condition,
    *                                 should this transition always fire (this should be true)
    *                                 in the state or never fire (false)
    * @param enableAlternatives If this is true, it is enough that at least one operation
    *                           condition is true for the transition to fire. The conditions
    *                           that are true, those actions will be executed.
    * @param onlyGuard If this transition should not execute the actions in the condition
    *                  This is used in some cases when for example is going from not enabled to
    *                  enabled
    * @param id The id of the transition
    */
  case class OperationTransition(states: Set[SPValue],
                                 conditionKind: Set[SPValue],
                                 nextState: SPValue,
                                 event: Option[SPValue] = None,
                                 alwaysTrueIfNoConditions: Boolean = true,
                                 enableAlternatives: Boolean = false,
                                 onlyGuard: Boolean = false,
                                 negateGuard: Boolean = false,
                                 id: ID = ID.newID
                                )


  /**
    * The result from a run.
    * @param lastState
    * @param sequence
    */
  case class OneOperationRun(lastState: SPState, sequence: List[(Operation, SPState)], newPlan: List[String])

  case class FireEvent(event: SPValue, operation: ID)

  // Add force when we need it
  //case class ForceTransition(conditionKind: SPValue, operation: ID)

  /**
    * This method runs the operations one step. only one op transition will be executed
    * per step.
    *
    * Comment: If we need to run many operations
    * like > 5000, we need to improve the logic and not reevaluate possible transitions.
    * But for now
    * @param ops The operations
    * @param s The state
    * @param fire What event that should be fired
    * @param controlledTransitions The transitions that must be activated with an event
    * @param unControlledTransitions Transition that will executed if they are enabled
    * @param events The event definition
    * @param disabledGroups If some condition groups should be disabled
    * @return
    */
  final def runOperations(ops: List[Operation],
                          s: SPState,
                          fire: List[FireEvent],
                          controlledTransitions: List[OperationTransition],
                          unControlledTransitions: List[OperationTransition],
                          disabledGroups: Set[SPValue] = Set(),
                          plan: List[String] = List(), stateHasChanged: Boolean = false, model: List[IDAble]
                         ): OneOperationRun =  {


    @tailrec
    def runDeep(ops: List[Operation],
                s: SPState,
                aggr: List[(Operation, SPState)] = List()
               ): List[(Operation, SPState)] = {

      val enabledOps = ops.flatMap { op =>
        val enabledTs = possibleTransitions(op, s, fire, controlledTransitions, unControlledTransitions).find(t =>
          evaluateOP(op, s, t.conditionKind, disabledGroups, t.alwaysTrueIfNoConditions, t.enableAlternatives, t.negateGuard)
        )
        enabledTs.map(op -> _)
      }

      enabledOps match {
        case Nil => aggr
        case x :: xs =>
          val op = x._1
          val t = x._2
          val nextState = takeTransition(op, s, t.conditionKind, t.nextState, disabledGroups, t.enableAlternatives, t.onlyGuard)
          runDeep(ops.filter(_ != op), nextState, (op, nextState) :: aggr)
      }

    }

    def plansToLTL(plans: List[String]): String = {
      val s = plans.map(p => s"(F ($p))").mkString("&")
      s"! ($s)"
    }

    val res = ops.foldLeft((s,plan)) { (acum, op) =>
      // hack this for now
      val s = acum._1
      val plan = acum._2

      val goalStates = ID.makeID("c3eebb27-1181-4282-b384-0311ce3fcbe2").get
      val currentGoals = s.state.get(goalStates).flatMap(spval => spval.asOpt[Map[ID, SPValue]]).getOrElse(Map())

      // plan for abilities active only in auto
      val isInAuto = unControlledTransitions.exists(_.id==AbilityRunnerTransitions.AbilityTransitions.enabledToStarting.id)

      // high level op
      val isOp = op.attributes.getAs[Boolean]("hasGoal").getOrElse(false)

      if(isOp) {
        val enabled = filterConditions(op.conditions, Set("pre"), Set())
        val post = filterConditions(op.conditions, Set("post"), Set())
        val isEnabled = enabled.forall(p => p.eval(s))
        if(s.get(op.id) != Some(SPValue("resetting")) && s.get(op.id) != Some(SPValue("enabled")) && (/*!isInAuto && */ fire.exists(f=>f.event == SPValue("reset") && f.operation==op.id))) {
          val resetGoal = AND(enabled.map(_.guard))
          val resetGoalStr = SPpropTonuXmvSyntax(resetGoal,
            model.collect{case v:Thing=>v}.map(v => v.copy(name = "v_"+v.name.replaceAll("\\.", "_"))),         /// TODO: terrible
            model.collect{case o:Operation=>o}.map(o => o.copy(name = "o_"+o.name.replaceAll("\\.", "_"))))
          println("updGoals: " + resetGoalStr)
          // high level plan operations can always be reset
          // update global goal state
          val updGoals = currentGoals + (op.id -> SPValue(resetGoalStr))
          println("updGoals (start reset): " + updGoals)
          val ns = s.next(op.id -> SPValue("resetting")).next(goalStates -> SPValue(updGoals))
          // 2. compute plan on new state
          val planSpec = plansToLTL(updGoals.values.toList.flatMap(v=>v.asOpt[String]))
          println("planspec: " + planSpec)
          val (plan, ntrans, stdout, stderr) = computePlan(model, ns.state, 50, AND(List()), planSpec, "/tmp/reset.smv")
          // 3. replace the current plan
          val np = plan
          (ns, np)
        } else if(s.get(op.id) == Some(SPValue("enabled")) && isEnabled && (/*isInAuto || i think we want "levels" of auto*/fire.exists(f=>f.event == SPValue("start") && f.operation==op.id))) {
          // high level plan operations can always start.
          // 1. take the pre action transition
          val ns = (enabled.foldLeft(s){(tempS, cond) => cond.next(tempS)}).next(op.id -> SPValue("executing"))
          // 2. compute plan on new state
          val goal = AND(post.map(_.guard))

          val goalStr = SPpropTonuXmvSyntax(goal,
            model.collect{case v:Thing=>v}.map(v => v.copy(name = "v_"+v.name.replaceAll("\\.", "_"))),         /// TODO: terrible
            model.collect{case o:Operation=>o}.map(o => o.copy(name = "o_"+o.name.replaceAll("\\.", "_"))))
          println("current goal: " + goalStr)
          // high level plan operations can always reset when not enabled
          // update global goal state
          val updGoals = currentGoals + (op.id -> SPValue(goalStr))
          println("updGoals (starting): " + updGoals)
          val ns2 = ns.next(goalStates -> SPValue(updGoals))
          // 2. compute plan on new state
          val planSpec = plansToLTL(updGoals.values.toList.flatMap(v=>v.asOpt[String]))
          println("planspec: " + planSpec)
          val (plan, ntrans, stdout, stderr) = computePlan(model, ns.state, 50, AND(List()), planSpec, "/tmp/runner.smv")
          // 3. replace the current plan
          val np = plan
          (ns2, np)
        } else if(s.get(op.id) == Some(SPValue("executing")) && post.forall(p => p.eval(s))) {
          // take finish transition
          println("taking finish transition for operation: " + op.name)
          val updGoals = currentGoals - op.id
          println("updGoals (finish op): " + updGoals)
          val ns = (post.foldLeft(s){(tempS, cond) => cond.next(tempS)}).next(op.id -> SPValue("finished")).next(goalStates -> SPValue(updGoals))
          (ns, plan)
        } else if(s.get(op.id) == Some(SPValue("executing"))) {
          // do nothing
          (s, plan)
        }
        else if(s.get(op.id) == Some(SPValue("resetting")) && isEnabled) {
          println("resetting done, back to enabled! clearing the goal")
          val updGoals = currentGoals - op.id
          println("updGoals after reset: " + updGoals)
          (s.next(op.id -> SPValue("enabled")).next(goalStates -> SPValue(updGoals)), plan)
        } else if(s.get(op.id) == Some(SPValue("resetting"))) {
          (s, plan)
        }
        else if(s.get(op.id) == Some(SPValue("notEnabled")) && isEnabled) {
          (s.next(op.id -> SPValue("enabled")), plan)
        }
        else if(s.get(op.id) == Some(SPValue("enabled")) && isEnabled) {
          (s, plan)
        }
        else
          (s.next(op.id -> SPValue("notEnabled")), plan)
      } else {
        val enabled = filterConditions(op.conditions, Set("pre"), Set())
        val executing = filterConditions(op.conditions, Set("isExecuting"), Set())
        val finished = filterConditions(op.conditions, Set("isFinished"), Set())

        val (ns, np) = if(enabled.forall(p => p.eval(s)) && ((!isInAuto && fire.exists(f=>f.event == SPValue("start") && f.operation==op.id)) ||
          (isInAuto && plan.headOption.contains(op.name)))) {
          // take start transition
          println("taking start transition for ability: " + op.name + " auto: " + isInAuto)
          val newPlan = if(isInAuto) plan.tail else plan
          (enabled.foldLeft(s){(tempS, cond) => cond.next(tempS)}, newPlan)
        } else if(s.get(op.id) != Some(SPValue("finished")) && finished.forall(p => p.eval(s))) {
          // take finish transition
          println("taking finish transition for ability: " + op.name)

          // re-plan after finishing...
          val np = if(currentGoals.values.isEmpty) plan else {
            val planSpec = plansToLTL(currentGoals.values.toList.flatMap(v=>v.asOpt[String]))
            println("planspec: " + planSpec)
            val (newPlan, ntrans, stdout, stderr) = computePlan(model, s.state, 50, AND(List()), planSpec, "/tmp/runner.smv")
            newPlan
          }

          (finished.foldLeft(s){(tempS, cond) => cond.next(tempS)}, np)
        } else (s, plan)

        val x = if(finished.forall(p => p.eval(ns)))
          ns.next(op.id -> SPValue("finished"))
        else if(executing.forall(p => p.eval(ns)))
          ns.next(op.id -> SPValue("executing"))
        else if(enabled.forall(p => p.eval(ns)))
          ns.next(op.id -> SPValue("enabled"))
        else
          ns.next(op.id -> SPValue("notEnabled"))

        (x, np)

      }

    }

    // val res = runDeep(ops, s, List())
    val lastState = res._1 // res.headOption.map(_._2).getOrElse(s)
    OneOperationRun(lastState, List(), res._2)
  }



  def possibleTransitions(op: Operation,
                          s: SPState,
                          fire: List[FireEvent],
                          controlledTransitions: List[OperationTransition],
                          unControlledTransitions: List[OperationTransition],
                         ): List[OperationTransition] = {

    val current = s.get(op.id).toList
    val tControlled = current.flatMap(c => controlledTransitions.filter{t =>
      val eventHasFired = t.event.forall(e => fire.exists(xs => xs.event == e && xs.operation == op.id))
      t.states.contains(c) && eventHasFired
    })
    val tUnControlled = current.flatMap(c => unControlledTransitions.filter(_.states.contains(c)))


    val ts = tControlled ++ tUnControlled

    // Just in case of messing up while implementing
    if (s.get(op.id).isEmpty)
      println(s"ERROR ERROR, You forgot to add the operation state to the state for operation: ${op.name}, id: ${op.id}")
//    else if (ts.isEmpty) {  // with events this is normal. Keeping printout if we need for troubleshooting
//      val avS = (controlledTransitions ++ unControlledTransitions).map(_.state)
//      println(s"ERROR ERROR, You have ended up in a state that has no transitions. ")
//      println(s"You are in state: ${s.state(op.id)}, but only have transitions from $avS, op: ${op.name}, id: ${op.id}")
//    }
    ts
  }


  /**
    * Takes a transition based on the current state of the operation. This method do not care about if the transition
    * is enabled or not (however it checks it if enableAlternatives is true). So always call evaluateOP before using
    * this method.
    *
    * @param op The operation
    * @param s The current state to update
    * @param kind The condition kinds to evaluate, i.e pre, post, reset, starting etc.
    * @param nextOPState The state that the operation will be in after the action, i.e. executing, finished, ...
    * @param disabledGroups The condition groups that should not be part of the evaluation
    * @param enableAlternatives Used for special conditions where only the actions for conditions
    *                           that is enabled will be executed.
    * @return an updated state or the same if nothing happened
    */
  def takeTransition(op: Operation,
                     s: SPState,
                     kind: Set[SPValue],
                     nextOPState: SPValue,
                     disabledGroups: Set[SPValue] = Set(),
                     enableAlternatives: Boolean = false,
                     onlyGuard: Boolean = false
                    ): SPState = {

    println("TAKING TRANSITION OF KIND: " + kind.toString)
    val preF = filterConditions(op.conditions, kind, disabledGroups)
    val filtered = if (enableAlternatives) preF.filter(_.eval(s)) else preF

    val newState = if (!onlyGuard) {
      filtered.foldLeft(s){(tempS, cond) => cond.next(tempS)}
    } else s
    newState.next(op.id -> nextOPState)
  }


  /**
    * Evaluates an operation if it can take a transition based on the current state
    * @param op The operation,
    * @param s The current state,
    * @param kind The condition kinds to evaluate, i.e pre, post, reset, starting etc.
    * @param disabledGroups The condition groups that should not be part of the evaluation
    * @param alwaysTrueIfNoConditions If the operation should make the transition even if there are now
    *                                 conditions of that kind. Usually true for normal conditions but false
    *                                 for e.g. reset
    * @param enableAlternatives Used for special conditions where not all conds needs to be enabled and where only
    *                           the actions for that condition will be executed.
    * @return if it can make a transition for the given condition kind
    */
  def evaluateOP(op: Operation,
                 s: SPState,
                 kind: Set[SPValue],
                 disabledGroups: Set[SPValue] = Set(),
                 alwaysTrueIfNoConditions: Boolean = true,
                 enableAlternatives: Boolean = false,
                 negateGuard: Boolean = false,
                ): Boolean = {
    val filtered = filterConditions(op.conditions, kind, disabledGroups)
    val ifNoConds = filtered.nonEmpty || alwaysTrueIfNoConditions
    val enabled = if (enableAlternatives) filtered.exists(p => p.eval(s)) else filtered.forall(p => p.eval(s))

    (enabled != negateGuard) && ifNoConds // XOR
  }

  /**
    * Filters a list of conditions based on its kind and group.
    * @param conds The list of conditions
    * @param set The kinds that we want (i.e. "pre")
    * @param disabledGroups The groups we do NOT want (usually defined as a string)
    * @return the filtered list
    */
  def filterConditions(conds: List[Condition],
                       kind: Set[SPValue],
                       disabledGroups: Set[SPValue] = Set()
                      )  = {
    conds.filter(c => {
      val notDisabledGroup = c.attributes.get("group").forall(g => !disabledGroups.contains(g))
      val res = c.attributes.get("kind").getOrElse(SPValue(""))
      ((kind contains res) || kind.isEmpty) && notDisabledGroup
    })
  }

}
