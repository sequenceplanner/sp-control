package spgui.widgets.labkit

import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration._
import scala.scalajs.js.timers._
import sp.domain._
import sp.domain.Logic._

class PMComm(onChannelUp: Option[() => Unit],
  onMessage: Option[(SPHeader,sp.patrikmodel.API.Response) => Unit]) {
  import spgui.communication.{BackendCommunication => bc }

  val api = sp.patrikmodel.API
  private var reqs: Map[ID, (Promise[api.Response], SetTimeoutHandle)] = Map() // order of declaration important!

  private val pmHandler = bc.getMessageObserver(pmHandlerCB, api.topicResponse)
  private val channelObserver = bc.getWebSocketStatusObserver(up => if(up) onChannelUp.foreach(_.apply), api.topicResponse)

  private def pmHandlerCB(mess: SPMessage): Unit = {
    val header = mess.header.to[SPHeader].getOrElse(SPHeader())
    mess.body.to[api.Response].foreach(resp => onMessage.foreach { f=>f(header, resp) })

    for {
      (p, timer) <- reqs.get(header.reqID)
      b <- mess.body.to[api.Response].toOption
    } yield { // request completed in time
      reqs -= header.reqID
      clearTimeout(timer)
      p.success(b)
    }
  }

  def ask(header: SPHeader, body: api.Request, timeout: FiniteDuration = 1000 millis): Future[api.Response] = {
    val h = header.copy(to = api.service)
    val msg = SPMessage.make[SPHeader, api.Request](h, body)
    bc.publish(msg, api.topicRequest)
    val p = Promise[api.Response]()
    val t = setTimeout(timeout) {
      reqs -= header.reqID
      p.failure(new java.util.concurrent.TimeoutException("No reply to request " + h.reqID + " from service in " + timeout.toSeconds + "s"))
    }
    reqs += (h.reqID -> (p, t))
    p.future
  }

  def tell(header: SPHeader, body: api.Request): Unit = {
    val h = header.copy(to = api.service)
    val msg = SPMessage.make[SPHeader, api.Request](h, body)
    bc.publish(msg, api.topicRequest)
  }

  def kill = pmHandler.kill()
}
