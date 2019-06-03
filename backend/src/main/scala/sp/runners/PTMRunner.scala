package sp.runners

import sp.domain._
import sp.domain.Logic._
import akka.actor.{Actor, Props}
import sp.runners.PTM_Models.StatePredicate

import sp.domain.logic.PropositionParser

case class PTMRunnerSetState(state: Option[SPState] = None,
                          ops: Option[List[PTM_Models.PTMOperation]] = None,
                          abs: Option[List[PTM_Models.PTMOperation]] = None,
                          opsQ: Option[List[ID]] = None,
                          absQ: Option[List[ID]] = None,
                          model: Option[List[IDAble]] = None,
                          pause: Option[Boolean] = None,
                          step: Option[Option[Boolean]] = None,
                          forceState: Option[Map[ID, SPValue]] = None,
                          forceGoal: Option[Option[String]] = None,
                         )

case class PTMRunnerState(state: SPState,
                          ops: List[PTM_Models.PTMOperation],
                          abs: List[PTM_Models.PTMOperation],
                          opsQ: List[ID],
                          absQ: List[ID],
                          model: List[IDAble],
                          pause: Boolean = true,
                          step: Option[Boolean] = None,
                          forceState: Map[ID, SPValue],
                          forceGoal: Option[String] = None,
                          predicates: List[StatePredicate],
                      )
object PTMRunnerState {
  def empty = PTMRunnerState(SPState("empty", Map()), List(), List(), List(), List(), List(), true, None, Map(), None, List())
}



object PTMRunnerActor {
  def props(init: PTMRunnerSetState) = Props(classOf[PTMRunnerActor], init)
}

class PTMRunnerActor(initialRunnerState: PTMRunnerSetState) extends Actor {
  import PTM_Models._

  var internal = PTMRunnerState.empty
  setInternalState(initialRunnerState)

  var prevState: Map[ID, SPValue] = Map()
  var prevGoals: Proposition = AND(List())

  override def receive = {

    case s: SPState =>  // Only allowed to be called via the flow!

      val incomingState = updState(internal.state.next(s.state), internal.predicates, internal.forceState)

      val planningOps = if (!internal.pause && internal.forceGoal.isEmpty)
        runOps(incomingState, internal.ops, internal.opsQ, internal.predicates, internal.forceState, true)
      else
        runPause(incomingState, internal.ops, internal.opsQ)
      val opsPredicates = evaluatePredicates(planningOps.updS, internal.ops)

      val (newGoals, goalStr) = if(internal.forceGoal.nonEmpty) {
        val gs = internal.forceGoal.get
        PropositionParser(internal.model).parseStr(gs) match {
          case Right(p) => (p, prettyPrint(internal.model)(p))
          case Left(err) => (AND(List()), s"Error: $err")
        }
      } else {
        val g = makeGoal(opsPredicates)
        (g, prettyPrint(internal.model)(g))
      }

      val stateChanged = planningOps.updS.state.toSet.diff(prevState.toSet).nonEmpty || newGoals != prevGoals

      val plan = if(!stateChanged) Some(internal.absQ)
      else if(newGoals != AND(List())){
        // println("new goal: " + prettyPrint(internal.model)(newGoals))
        // Send away new planning for abs and ops with fire and forget. Also send out internal.predicates
        // *** planning in sync for testing
        findMeAPlanNuxmv(internal.abs, planningOps.updS, newGoals, internal.model)
      } else {
        Some(List())
      }
      internal = internal.copy(absQ = plan.getOrElse(List()))

      val abilities: OneStep = if(internal.step.contains(false) || (internal.pause && internal.forceGoal.isEmpty)) {
        val x = runOps(planningOps.updS, internal.abs, List(), internal.predicates, internal.forceState)
        x.copy(updQ = internal.absQ)
      } else
          runOps(planningOps.updS, internal.abs, internal.absQ, internal.predicates, internal.forceState)
      // we shouldn't "run paused" here, we need to be alive w.r.t abilities

      val absPredicates = evaluatePredicates(abilities.updS, internal.abs)

      // hacky map to old domain to make old frontend work...
      def predNameToAbState(s: String): String = {
        s match {
          case _ if s.contains("_pre") => "enabled"
          case _ if s.contains("_isExecuting") => "executing"
          case _ if s.contains("_isFinished") => "finished"
          case _  => "notEnabled"
        }
      }
      val updAbState = absPredicates.map { case (a,p) =>
        a.o.id -> SPValue(p.headOption.map(x=>predNameToAbState(x.name)).getOrElse("notEnabled"))
      }.toMap
      val ns = abilities.updS.next(updAbState)
      def toOldOpState(s: String): String = {
        s match {
          case "i" => "enabled"
          case "e" => "executing"
          case "f" => "finished"
          case x => println("x: " + x); "notEnabled"
        }
      }
      val updOpState = internal.model.collect {
        case t: Thing if t.attributes.getAs[ID]("op").nonEmpty => (t.id, t.attributes.getAs[ID]("op").get)
      }.flatMap { case (tid,opid) =>
          ns.get(tid).map{spval => (opid -> SPValue(toOldOpState(spval.as[String]))) }
      }.toMap

      internal = internal.copy(
        state = ns.next(updOpState),
        step = internal.step.map(_ => false),
        opsQ = planningOps.updQ,
        absQ = abilities.updQ
      )

      val fired = planningOps.fired ++ abilities.fired // We should send this to someone nice
      if (planningOps.fired.nonEmpty) println(s"the following transition fired: ${planningOps.fired.map(_.name)}")
      // Maybe also send out if a que was changed?
      // Send out the state internal.predicates or maybe we should include them in the state?

      // we love hacks :) send the current plan and goal with the state
      val queueNames = if(plan.isEmpty) List("no plan found...")
      else if(plan.contains(List())) List("empty plan")
      else {
        internal.absQ.flatMap { id =>
          internal.abs.flatMap(a => a.controlled.find(_.id == id).map(_.name))
        }
      }

      val qs = (ID.newID -> SPAttributes("q" -> queueNames, "goal" -> goalStr))

      val newState = internal.state.next(qs)

      // only replan when state has actually changed...
      prevState = newState.state
      prevGoals = newGoals

      sender() ! newState


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
      step = if (set.step.nonEmpty) set.step.get else internal.step,
      forceState = if (set.forceState.nonEmpty) {
        // hack to force operations from old frontend
        val fs = set.forceState.get
        val mapping = internal.model.collect {
          case t: Thing if t.attributes.getAs[ID]("op").nonEmpty => (t.attributes.getAs[ID]("op").get, t.id)
        }.toMap
        fs.map { case (i,v) if mapping.contains(i) => mapping(i) -> v
          case (i,v) => i -> v
        }
      }else internal.forceState,
      forceGoal = if (set.forceGoal.nonEmpty) set.forceGoal.get else internal.forceGoal,
      predicates = List()
    )
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
    val o = op.getOrElse(Operation("OP_"+name))
    val variable = Thing(name, SPAttributes("domain" -> List("i", "e", "f"), "initialState" -> "i", "op" -> o.id)) // TODO: hack to be able to use old style state in frontend
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
        action = Action(variable.id, ValueHolder("i")) +: goal.action,   /// TODO: hack to reset ops for the demo
        attributes = goal.attributes + ("kind" -> "post")
      ),
      "post"
    )
    (PTMOperation(List(init, execute), List(pre), List(post), List(), o), variable)
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

  def findMeAPlanNuxmv(ops: List[PTMOperation], state: SPState, goal: Proposition, model: List[IDAble]): Option[List[ID]] = {
    val goalStr = SPpropTonuXmvSyntax(goal,
      model.collect{case v:Thing=>v}.map(v => v.copy(name = "v_"+v.name.replaceAll("\\.", "_"))),         /// TODO: terrible
      model.collect{case o:Operation=>o}.map(o => o.copy(name = "o_"+o.name.replaceAll("\\.", "_"))))

    val planSpec = s"! F ( $goalStr )"
    val (plan, ntrans, stdout, stderr) = computePlan(model, state.state, 50, AND(List()), planSpec, "/tmp/ptmrunner.smv")

    if(ntrans == -1) None
    else Some(
      plan.flatMap { name =>
        ops.find(_.o.name == name).flatMap { o => o.controlled.headOption.map(_.id) }
      }
    )
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
