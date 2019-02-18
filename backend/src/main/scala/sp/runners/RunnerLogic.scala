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

  def computePlan(model: List[IDAble], state: Map[ID, SPValue], goal: Proposition, filename: String = "/tmp/runner.smv"): List[String] = {
    exportNuXmv(model, filename, state, goal, "")

    // TODO: export command file
    // then run it as: echo quit | nuxmv -source test.txt /tmp/runner.sm
    //  val msg = "to stdin of process"; out.write(msg.getBytes); out.close;
    // val process = new ProcessIO(writeJob, readJob, errJob)

    import sys.process._
    import java.io.{ByteArrayOutputStream,PrintWriter}
    def runCommand(cmd: String): (Int, String, String) = {
      val stdoutStream = new ByteArrayOutputStream
      val stderrStream = new ByteArrayOutputStream
      val stdoutWriter = new PrintWriter(stdoutStream)
      val stderrWriter = new PrintWriter(stderrStream)
      val exitValue = cmd.!(ProcessLogger(stdoutWriter.println, stderrWriter.println))
      stdoutWriter.close()
      stderrWriter.close()
      (exitValue, stdoutStream.toString, stderrStream.toString)
    }

    println("Running solver...")
    val t0 = System.nanoTime()
    val (result, stdout, stderr) = runCommand(s"/home/martin/bin/nuxmv -bmc -bmc_length 50 $filename")
    val t1 = System.nanoTime()
    println("Time to solve: " + (t1 - t0) / 1e9d + " seconds. Error code: " + result)
    if(result == 0) {
      val s = stdout.replaceAll("(?m)^\\*\\*\\*.*?\n", "")
      val plan = s.split("\n").toList.filter(_.contains("_start = TRUE")).map(_.trim)
      val ops = model.collect { case op: Operation => op }
      val opNames = ops.map(o => "o_"+o.name.replaceAll("\\.", "_") -> o.name).toMap
      val p = plan.flatMap( str => opNames.get(str.stripSuffix("_start = TRUE")))
      println("New plan is: " + p.mkString(","))
      p
    } else {
      println("Failed to compute plan!")
      List()
    }
  }

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

    val res = ops.foldLeft((s,plan)) { (acum, op) =>
      // hack this for now
      val s = acum._1
      val plan = acum._2
      val enabled = filterConditions(op.conditions, Set("pre"), Set())
      val executing = filterConditions(op.conditions, Set("isExecuting"), Set())
      val finished = filterConditions(op.conditions, Set("isFinished"), Set())

      // if(enabled.forall(p => p.eval(s)) && plan.headOption.contains(op.name)) {
      //   println("plan allows: " + op.name)
      // } else { println("plan is: " + plan.mkString(",")) }

      // plan active only in auto
      val isInAuto = unControlledTransitions.exists(_.id==AbilityRunnerTransitions.AbilityTransitions.enabledToStarting.id)

      val opGoal = op.attributes.getAs[String]("hasGoal").getOrElse("")

      // reset high level op => enable the "pre" state!
      val (ns, np) = if(opGoal.nonEmpty && !enabled.forall(p => p.eval(s)) && (!isInAuto && fire.exists(f=>f.event == SPValue("reset") && f.operation==op.id))) {
        val resetGoal = AND(enabled.map(_.guard))
        // high level plan operations can always start.
        // 1. dont take the pre transition. in fact
        val ns = s
        // 2. compute plan on new state
        val plan = computePlan(model, ns.state, resetGoal, "/tmp/reset.smv")
        // 2. for now just replace the current plan
        val np = plan
        (ns, np)
      } else if(opGoal.nonEmpty && enabled.forall(p => p.eval(s)) && (isInAuto || fire.exists(f=>f.event == SPValue("start") && f.operation==op.id))) {
        // high level plan operations can always start.
        // 1. take the pre action transition
        val ns = enabled.foldLeft(s){(tempS, cond) => cond.next(tempS)}
        // 2. compute plan on new state
        val goal = AND(finished.map(_.guard))
        val plan = computePlan(model, ns.state, goal)
        // 2. for now just replace the current plan
        val np = plan
        (ns, np)
      } else if(opGoal.isEmpty && enabled.forall(p => p.eval(s)) && ((!isInAuto && fire.exists(f=>f.event == SPValue("start") && f.operation==op.id)) ||
        (isInAuto && plan.headOption.contains(op.name)))) {
        // take start transition
        println("taking start transition for: " + op.name + " auto: " + isInAuto)
        val newPlan = if(isInAuto) plan.tail else plan
        (enabled.foldLeft(s){(tempS, cond) => cond.next(tempS)}, newPlan)
      } else if(s.get(op.id) != Some(SPValue("finished")) && finished.forall(p => p.eval(s))) {
        // take finish transition
        println("taking finish transition for: " + op.name)
        (finished.foldLeft(s){(tempS, cond) => cond.next(tempS)}, plan)
      } else (s, plan)

      val x = if(finished.forall(p => p.eval(ns)))
        ns.next(op.id -> SPValue("finished"))
      else if(executing.forall(p => p.eval(ns)))
        ns.next(op.id -> SPValue("executing"))
      else if(enabled.forall(p => p.eval(ns)))
        ns.next(op.id -> SPValue("enabled"))
      else ns.next(op.id -> SPValue("notEnabled"))

      (x, np)
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
