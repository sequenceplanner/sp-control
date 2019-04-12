package sp.runners

import sp.domain._
import sp.domain.Logic._
import PTM_Models._
import akka.actor.{Actor, Props}
case class PTMRunnerState(state: Option[SPState] = None,
                          ops: List[PTMOperation] = List(),
                          abs: List[PTMOperation] = List(),
                          opsQ: List[ID] = List(),
                          absQ: List[ID] = List(),
                          //disabledGroups: Set[SPValue],
                          pause: Option[Boolean] = None
                      )


object PTMRunnerActor {
  def props(init: PTMRunnerState) = Props(classOf[PTMRunnerActor], init)
}
class PTMRunnerActor(initialRunnerState: PTMRunnerState) extends Actor {

  var internal = initialRunnerState
  var state = initialRunnerState.state.getOrElse(SPState("noState", Map()))
  var pause = initialRunnerState.pause.getOrElse(false)
  var prevState = state // remember when the state changes


  override def receive = {

    case s: SPState =>  // Only allowed to be called via the flow!

      val planningOps = runOps(state.next(s.state), internal.ops, internal.opsQ)
      internal = internal.copy(opsQ = planningOps.updQ)
      val opsPredicates = evaluatePredicates(planningOps.updS, internal.ops)

      //val newGoals =
      // Send away new planning for abs and ops with fire and forget. Also send out predicates

      val abilities = runOps(planningOps.updS, internal.abs, internal.absQ)
      state = abilities.updS
      internal = internal.copy(state = Some(state), absQ = abilities.updQ)
      val absPredicates = evaluatePredicates(abilities.updS, internal.abs)

      val fired = planningOps.fired ++ abilities.fired // We should send this to someone nice
      if (fired.nonEmpty) println(s"the following transition fired: ${fired.map(_.name)}")
      // Maybe also send out if a que was changed?

      sender() ! state


    case x: PTMRunnerState =>
      // just a simple way to handle update of internals. To be used by planners
      state = x.state.getOrElse(state)
      pause = x.pause.getOrElse(pause)
      internal = PTMRunnerState(
        state = Some(state), // replace complete state
        ops = if (x.ops.nonEmpty) x.ops else internal.ops,
        abs = if (x.abs.nonEmpty) x.abs else internal.abs,
        opsQ = if (x.opsQ.nonEmpty) x.opsQ else internal.opsQ,
        absQ = if (x.absQ.nonEmpty) x.absQ else internal.absQ,
        pause = Some(pause)
      )

  }


}








object PTM_Models {
  case class StatePredicate(name: String, predicate: Proposition)
  case class PTMTransition(condition: Condition, name: String = "", id: ID = ID.newID)

  case class PTMOperation(predicates: List[StatePredicate],
                          controlled: List[PTMTransition],
                          unControlled: List[PTMTransition],
                          effects: List[PTMTransition],
                          //o: Operation
                         )
  case class ControlQue(xs: List[ID])
  case class OneStep(updS: SPState, updQ: List[ID], fired: List[PTMTransition])


  def runOps(s: SPState, ops: List[PTMOperation], q: List[ID]) = {
    val cs = ops.flatMap(_.controlled)
    val us = ops.flatMap(_.unControlled)
    val res = runOneStepSeq(s, cs, us, ControlQue(q))
    OneStep(res._1, res._2.xs, res._3)
  }

  def evaluatePredicates(s: SPState, xs: List[PTMOperation]) = {
    xs.map(o =>  o -> o.predicates.filter(_.predicate.eval(s))).toMap
  }

  /**
    * This runs one controlled transition and multiple uncontrolled. Currently runs all based on
    * the given state.
    * @param state The input state
    * @param controlled A list of controlled transition that only starts based on the que
    * @param unControlled A list of uncontrolled transitions that starts when they are enabled
    * @param que A list of transition ids defining the starting order
    * @return A new state, que and list of fired transitions (just nu namn, sedan id)
    *         (and returns the same if they where not updated)
    */
  def runOneStep(state: SPState,
                 controlled: List[PTMTransition],
                 unControlled: List[PTMTransition],
                 que: ControlQue): (SPState, ControlQue, List[PTMTransition]) = {

    // Some initial test that we can remove or do something with later
    que.xs.foreach{t =>
      if (!controlled.exists(c => c.id == t) ) println(s"transition $t in the que, does not exist in the controlled list")
    }

    val cT = for {
      toStart <- que.xs.headOption
      transition <- controlled.find(_.id == toStart)
      if transition.condition.eval(state)
    } yield transition

    val tryControlledTransition = cT.map(t => next(t.condition, state)) // take step if controlled transition is enabled
    val updQue = if (tryControlledTransition.isDefined) ControlQue(que.xs.tail) else que // remove head in que if transition taken

    val stateAfterUncontrolled = unControlled.foldLeft((Map[ID, SPValue](), cT.toList)){case (sl, t) =>
      if (t.condition.eval(state)){
        val newS = next(t.condition, state)
        // check so that we do not overwrite when newS ++ sl._1
        (newS ++ sl._1, sl._2 :+ t)
      } else
        sl
    }

    // Maybe we should return only changed values?
    val newS = stateAfterUncontrolled._1 ++ tryControlledTransition.getOrElse(Map())

    (state.next(newS), updQue, stateAfterUncontrolled._2)
  }



  // Runs all transition in sequence
  def runOneStepSeq(state: SPState,
                    controlled: List[PTMTransition],
                    unControlled: List[PTMTransition],
                    que: ControlQue): (SPState, ControlQue, List[PTMTransition]) = {

    // Some initial test that we can remove or do something with later
    que.xs.foreach{t =>
      if (!controlled.exists(c => c.id == t) ) println(s"transition $t in the que, does not exist in the controlled list")
    }

    val cT = for {
      toStart <- que.xs.headOption
      transition <- controlled.find(_.id == toStart)
      if transition.condition.eval(state)
    } yield transition

    val tryControlledTransition = cT.map(_.condition.next(state))
    val updQue = if (tryControlledTransition.isDefined) ControlQue(que.xs.tail) else que // remove head in que if transition taken

    val stateAfterControlled = tryControlledTransition.getOrElse(state)
    val stateAfterUncontrolled = unControlled.foldLeft(stateAfterControlled, cT.toList){case (sl, t) =>
      if (t.condition.eval(sl._1)){
        (t.condition.next(sl._1), sl._2 :+ t)
      } else
        sl
    }

    (stateAfterUncontrolled._1, updQue, stateAfterUncontrolled._2)
  }

  // returns only changed. Maybe better
  def next(c: Condition, s: SPState): Map[ID, SPValue] = {
    c.action.map(a => a.id -> a.nextValue(s)).toMap
  }


}