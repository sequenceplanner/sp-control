package sp.runners

import sp.domain._
import Logic._

import scala.annotation.tailrec

trait RunnerLogic {
  type State = Map[ID, SPValue]
  case class OperationTransition(state: SPValue,
                                 conditionKind: SPValue,
                                 nextState: SPValue,
                                 alwaysTrueIfNoConditions: Boolean = true,
                                 enableAlternatives: Boolean = false
                                )

  case class OperationEvent(event: SPValue,
                            enabledInStates: Set[SPValue],
                            conditionKind: SPValue
                           )

  case class OneOperationRun(lastState: SPState, sequence: List[(Operation, SPState)])


  final def runOperations(ops: List[Operation],
                          s: SPState,
                          fire: Set[SPValue],
                          controlledTransitions: List[OperationTransition],
                          unControlledTransitions: List[OperationTransition],
                          events: List[OperationEvent] = List(),
                          disabledGroups: Set[SPValue] = Set(),
                         ): OneOperationRun =  {


    @tailrec
    def runDeep(ops: List[Operation],
                s: SPState,
                aggr: List[(Operation, SPState)] = List()
               ): List[(Operation, SPState)] = {

      val enabledOps = ops.flatMap { op =>
        val enabledTs = possibleTransitions(op, s, fire, controlledTransitions, unControlledTransitions, events).find(t =>
          evaluateOP(op, s, Set(t.conditionKind), disabledGroups, t.alwaysTrueIfNoConditions, t.enableAlternatives)
        )
        enabledTs.map(op -> _)
      }

      enabledOps match {
        case Nil => aggr
        case x :: xs =>
          val op = x._1
          val t = x._2
          val nextState = takeTransition(op, s, Set(t.conditionKind), t.nextState, disabledGroups, t.enableAlternatives)
          runDeep(ops.filter(_ != op), nextState, (op, nextState) :: aggr)
      }

    }

    val res = runDeep(ops, s, List())
    val lastState = res.headOption.map(_._2).getOrElse(s)
    OneOperationRun(lastState, res)
  }



  def possibleTransitions(op: Operation,
                          s: SPState,
                          fire: Set[SPValue],
                          controlledTransitions: List[OperationTransition],
                          unControlledTransitions: List[OperationTransition],
                          events: List[OperationEvent]
                         ): List[OperationTransition] = {

    val current = s.get(op.id).toList
    val tControlled = current.flatMap(c => controlledTransitions.filter(_.state == c))
    val tUnControlled = current.flatMap(c => unControlledTransitions.filter(_.state == c))

    val es = current.flatMap(c => events.filter(e =>
      e.enabledInStates.contains(c))
    ).map(_.conditionKind)

    val ts = tControlled.filter(t => es.contains(t.conditionKind)) ++ tUnControlled

    // Just in case of messing up while implementing
    if (s.get(op.id).isEmpty)
      println(s"ERROR ERROR, You forgot to add the operation state to the state for operation: ${op.name}, id: ${op.id}")
    else if (ts.isEmpty) {
      val avS = (controlledTransitions ++ unControlledTransitions).map(_.state)
      println(s"ERROR ERROR, You have ended up in a state that has no transitions. ")
      println(s"You are in state: ${s.state(op.id)}, but only have transitions from $avS, op: ${op.name}, id: ${op.id}")
    }
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
                     enableAlternatives: Boolean = false
                    ): SPState = {

    val preF = filterConditions(op.conditions, kind, disabledGroups)
    val filtered = if (enableAlternatives) preF.filter(_.eval(s)) else preF

    val newState = filtered.foldLeft(s){(tempS, cond) => cond.next(tempS)}
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
                 enableAlternatives: Boolean = false
                ): Boolean = {
    val filtered = filterConditions(op.conditions, kind, disabledGroups)
    val ifNoConds = filtered.nonEmpty || alwaysTrueIfNoConditions
    val enabled = if (enableAlternatives) filtered.exists(p => p.eval(s)) else filtered.forall(p => p.eval(s))

    enabled && ifNoConds
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
