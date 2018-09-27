package sp.virtualdevice

import akka.NotUsed
import akka.actor._
//import akka.cluster.pubsub.DistributedPubSubMediator.Subscribed
import akka.stream._
import akka.stream.scaladsl._


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



class SPVirtualDevice {

}
