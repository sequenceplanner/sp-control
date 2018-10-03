package sp.runners

import sp.domain._
import Logic._
import akka.actor._
import akka.util._
import akka.stream._
import akka.stream.scaladsl._
import scala.concurrent.duration._
import scala.concurrent.Future

case class RunnerPipeline(operations: List[Operation],
                          transitionSystem: List[RunnerLogic.OperationTransition],
                          initialState: SPState,
                          name: String,
                          system: ActorSystem
                          ) {




  implicit val askTimeout = Timeout(5 seconds)
  import akka.pattern.ask
  val runnerA = system.actorOf(Props(classOf[RunnerActor], transitionSystem, initialState, operations))

  def runnerFlow(includeTicker: Option[FiniteDuration] = None) = {
    val ticker = includeTicker.map(t => Source.tick(t, t, StateUpd(SPState("tick", Map()), List()))).getOrElse(Source.empty[StateUpd])
    Flow[StateUpd]
      .merge(ticker)
      .ask[RunnerLogic.OneOperationRun](1)(runnerA)
      .mapConcat(x =>
        x.sequence.map(_._2).reverse
      )
  }


  // Below not tested!

  def getRunnerData: Future[SPAttributes] = {
    val res = (runnerA ? GetRunnerData).mapTo[RunnerState]

    res.map(i =>
      SPAttributes(
        "operations" -> i.ops,
        "controlledTransitions" -> i.controlledTransitions.map(_.id),
        "persistentEvents" -> i.persistentEvents.map(e => SPAttributes("event" -> e.event, "operation" -> e.operation)),
        "disabledGroups" -> i.disabledGroups
      )
    )(system.dispatcher)
  }

  def makeTransitionsControlled(transitions: List[ID]) = {
    runnerA ! MakeControlled(transitions)
  }


// Change this so the actor is doing the merge and not like this since we can have
// a race condition where this method overwrites a new state

//  def addOperations(ops: List[Operation], initialState: SPState) = {
//    (runnerA ? GetRunnerData).mapTo[RunnerState].map{rs =>
//      val newOps = rs.ops ++ ops distinct
//      val newS = rs.state.next(initialState.state)
//      runnerA ! SetRunnerData()
//    }(system.dispatcher)
//
//
//    runnerA ! MakeUnControlled(transitions)
//  }





  //def updateState(next: )


}







trait RunnerActorAPI
case object GetRunnerData extends RunnerActorAPI
case class MakeControlled(ts: List[ID]) extends RunnerActorAPI
case class MakeUnControlled(ts: List[ID]) extends RunnerActorAPI
case class SetRunnerData(data: RunnerState) extends RunnerActorAPI
case class StateUpd(s: SPState, events: List[RunnerLogic.FireEvent]) extends RunnerActorAPI
object StateUpd {
  def empty = StateUpd(SPState("empty", Map()), List())
}


case class RunnerState(state: SPState,
                       ops: List[Operation],
                       controlledTransitions: List[RunnerLogic.OperationTransition],
                       unControlledTransitions: List[RunnerLogic.OperationTransition],
                       persistentEvents: List[RunnerLogic.FireEvent],
                       disabledGroups: Set[SPValue]
                      )


class RunnerActor(transitionSystem: List[RunnerLogic.OperationTransition],
                  initialState: SPState,
                  operations: List[Operation]
                 ) extends Actor {


  var internal = RunnerState(
    state = initialState,
    ops = operations,
    controlledTransitions = transitionSystem.filter(_.event.nonEmpty),
    unControlledTransitions = transitionSystem.filter(_.event.isEmpty),
    persistentEvents = List(),
    disabledGroups = Set()
  )




  override def receive = {
    case StateUpd(s, es) =>
      // println("Tjo")
      // println(internal)

      val updS = internal.state.next(s.state)

      val res = RunnerLogic.runOperations(
        internal.ops,
        updS,
        es ++ internal.persistentEvents,
        internal.controlledTransitions,
        internal.unControlledTransitions,
        internal.disabledGroups
      )
      internal = internal.copy(state = res.lastState)
      // println("new state: " + internal.state)
      sender() ! res

    case GetRunnerData => sender() ! internal
      // change so we do merge here instead
//    case SetRunnerData(d) =>
//      internal = internal.copy(
//        ops = if (d.ops.nonEmpty) d.ops else internal.ops,
//        persistentEvents = if (d.persistentEvents.nonEmpty) d.persistentEvents else internal.persistentEvents,
//        disabledGroups = if (d.disabledGroups.nonEmpty) d.disabledGroups else internal.disabledGroups,
//        controlledTransitions = if (d.controlledTransitions.nonEmpty) d.controlledTransitions else internal.controlledTransitions,
//        unControlledTransitions = if (d.unControlledTransitions.nonEmpty) d.unControlledTransitions else internal.unControlledTransitions
//      )
    case MakeControlled(ts) =>
      val xs = transitionSystem.filter(t => ts.contains(t.id))
      val unc = internal.unControlledTransitions.filter(xs.contains)
      val c = (internal.controlledTransitions ++ xs).distinct
      internal = internal.copy(
        controlledTransitions = c,
        unControlledTransitions = unc
      )
    case MakeUnControlled(ts) =>
      val xs = transitionSystem.filter(t => ts.contains(t.id))
      val c = internal.controlledTransitions.filter(xs.contains)
      val unc = (internal.unControlledTransitions ++ xs).distinct
      internal = internal.copy(
        controlledTransitions = c,
        unControlledTransitions = unc
      )

  }


  private def validateState = {
    val opsState = internal.ops.map(o => o.id -> internal.state.get(o.id))
    if (opsState.exists(_._2.isEmpty)){
      internal = internal.copy(state = internal.state.next(opsState.map(kv => kv._1 -> kv._2.getOrElse(SPValue(""))).toMap))
    }
  }

}
