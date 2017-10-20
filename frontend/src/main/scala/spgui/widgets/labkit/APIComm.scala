package spgui.widgets.labkit

import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration._
import scala.scalajs.js.timers._
import sp.domain._
import sp.domain.Logic._

class APIComm[RQT,RST](requestTopic: String, responseTopic: String, from: String, to: String,
  onChannelUp: Option[() => Unit], onMessage: Option[(SPHeader,RST) => Unit])
  (implicit val writeRequest: JSFormat[RQT], implicit val readResponse: JSFormat[RST]) {
  import spgui.communication.{BackendCommunication => bc }

  private var reqs: Map[ID, (Promise[(SPHeader,RST)], SetTimeoutHandle)] = Map() // order of declaration important!

  private val topicHandler = bc.getMessageObserver(topicHandlerCB, responseTopic)
  private val channelObserver = bc.getWebSocketStatusObserver(up => if(up) onChannelUp.foreach(_.apply), responseTopic)

  private def topicHandlerCB(mess: SPMessage): Unit = {
    for {
      h <- mess.header.to[SPHeader].toOption
      b <- mess.body.to[RST](readResponse).toOption
    } yield {
      onMessage.foreach { f=>f(h, b) }

      reqs.get(h.reqID).foreach { case(p, timer) =>
        reqs -= h.reqID
        clearTimeout(timer)
        p.success((h,b))
      }
    }
  }

  def ask(body: RQT): Future[(SPHeader,RST)] = ask(SPHeader(from = from, to = to), body)

  def ask(header: SPHeader, body: RQT, timeout: FiniteDuration = 1000 millis): Future[(SPHeader,RST)] = {
    val msg = SPMessage.make[SPHeader, RQT](header, body)(implicitly, writeRequest)
    bc.publish(msg, requestTopic)
    val p = Promise[(SPHeader,RST)]()
    val t = setTimeout(timeout) {
      reqs -= header.reqID
      p.failure(new java.util.concurrent.TimeoutException("No reply to request " + header.reqID + " from service in " + timeout.toSeconds + "s"))
    }
    reqs += (header.reqID -> (p, t))
    p.future
  }

  def tell(body: RQT): Unit =
    tell(SPHeader(from = from, to = to), body)

  def tell(header: SPHeader, body: RQT): Unit = {
    val msg = SPMessage.make[SPHeader, RQT](header, body)(implicitly, writeRequest)
    bc.publish(msg, requestTopic)
  }

  def kill = topicHandler.kill()
}
