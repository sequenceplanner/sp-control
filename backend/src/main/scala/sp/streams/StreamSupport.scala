package sp.streams

import akka.NotUsed
import akka.actor._
import akka.cluster.pubsub.DistributedPubSubMediator._
import akka.stream._
import akka.stream.scaladsl._

import scala.concurrent.duration._

trait SPStreamSupport {
  implicit val system: ActorSystem

  //  def mediatorSource: Source[Any, NotUsed] = {
  //    val x = Source.actorRef[Any](100, OverflowStrategy.fail)
  //    val flow = Flow.fromFunction[Any, Any]{
  //      case Subscribed
  //    }
  //    Source.fr
  //  }



}

// TODO: Move this somewhere...
object SPStreamSupport {
  def mergeSources[T](sources: List[Source[T, _]]) = sources match {
    case Nil => Source.empty[T]
    case first :: Nil => first
    case first :: second :: Nil => Source.combine(first, second)(Merge[T](_))
    case first :: second :: rest =>
      Source.combine(first, second, rest:_*)(Merge[T](_))
  }

  def mergeSinks[T](sinks: List[Sink[T, _]]) = sinks match {
    case Nil => Sink.ignore
    case first :: Nil => first
    case first :: second :: Nil => Sink.combine(first, second)(Broadcast[T](_))
    case first :: second :: rest =>
      Sink.combine(first, second, rest:_*)(Broadcast[T](_))
  }

  def normalizeRate[T](interval: FiniteDuration): Flow[T, T, _] = {
    Flow[T].conflate((lastMessage, newMessage) => newMessage). // conflate to throw away excess elements
      extrapolate(Iterator.continually(_), None)               // extrapolate if there are too few elements
      .zip(Source.tick(interval, interval, ())).map(_._1)      // zip with a known rate
  }
}
