package sp.rosFrontendHelper

import akka.actor._
import sp.domain.Logic._
import sp.domain._
import sp.service.MessageBussSupport
import scala.concurrent.duration._

import akka.stream._
import akka.stream.scaladsl._
import akka.NotUsed

import sp.drivers.ros2._

import sp.rosFrontendHelper.{APIRosFrontendHelper => api }

object RosFrontendHelper {
  def props = Props(classOf[RosFrontendHelper])
}

class RosFrontendHelper extends Actor with MessageBussSupport {
  implicit val materializer = ActorMaterializer()(context.system)
  import context.dispatcher

  val ros = new RCLBase(context.system)
  var sub: Option[UniqueKillSwitch] = None

  subscribe(api.topicRequest)

  def receive: Receive = {
    case s : String =>
      for { // unpack message
        message <- SPMessage.fromJson(s)
        header <- message.getHeaderAs[SPHeader] // if  header.to == api.service
        body <- message.getBodyAs[api.Request]
      } yield {
        val responseHeader = header.swapToAndFrom()
        sendAnswer(SPMessage.makeJson(responseHeader, APISP.SPACK())) // acknowledge message received

        body match { // Check if the body is any of the following classes, and execute program
          case api.GetEmptyMessage(msgType: String) =>
            val empty = for {
              msg <- ROSHelpers.createROSMsg(msgType).map(ROSHelpers.msgToAttr)
            } yield msg
            println("GOT request for empty message: " + msgType + " sending back: " + empty)
            sendAnswer(SPMessage.makeJson(responseHeader, api.EmptyMessage(empty)))

          case api.Publish(msgType: String, topic: String, msg: SPAttributes) =>
            ROSHelpers.createROSMsg(msgType).map(ROSHelpers.msgToAttr).foreach { emptyMsg =>
              val toSend = emptyMsg ++ msg
              val p = ros.publisher(msgType, topic)
              Source.tick(initialDelay = 0.nanos, interval = 500.millis, tick = ()).map(_ => toSend).take(6).to(p).run()
            }

          case api.Subscribe(msgType: String, topic: String) =>
            // only sub if we can create a message of the type
            ROSHelpers.createROSMsg(msgType).map(ROSHelpers.msgToAttr).foreach { emptyMsg =>
              val source = ros.subscriber(msgType, topic)
              val sink = Sink.foreach[SPAttributes]{ attr =>
                sendAnswer(SPMessage.makeJson(responseHeader, api.RosMessage(topic, attr)))
              }
              sub.foreach(_.shutdown)
              sub = Some(source.viaMat(KillSwitches.single)(Keep.right).to(sink).run())
            }

          case api.StopSubscribe =>
            sub.foreach(_.shutdown)

          case _ => Unit
        }
        sendAnswer(SPMessage.makeJson(responseHeader, APISP.SPDone()))
      }
  }
  def sendAnswer(mess: String): Unit = publish(api.topicResponse, mess)
}
