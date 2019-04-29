package sp.runners

import akka.actor.{ActorSystem, Props}
import akka.stream.scaladsl._
import scala.concurrent.duration._
import scala.concurrent.Future
import akka.util.Timeout
import sp.domain._
import sp.domain.Logic._
import PTM_Models._

import scala.concurrent.Future

case class PTMRunnerPipeline(operations: List[PTMOperation],
                          abilities: List[PTMOperation],
                          initialState: SPState,
                          initialOperationTransitionQue: List[ID],
                          model: List[IDAble],
                          name: String,
                          system: ActorSystem
                         ) {




  implicit val askTimeout = Timeout(5 seconds)
  import akka.pattern.ask
  val runnerA = system.actorOf(PTMRunnerActor.props(PTMRunnerState(Some(initialState), operations, abilities, initialOperationTransitionQue, List(), model)))

  def runnerFlow =
    Flow[SPState].ask[SPState](runnerA)

  def getRunnerData: Future[PTMRunnerState] = (runnerA ? "GetTheData").mapTo[PTMRunnerState]
  def setRunnerData(data: PTMRunnerState): Future[Boolean] = (runnerA ? data).mapTo[Boolean]

}
