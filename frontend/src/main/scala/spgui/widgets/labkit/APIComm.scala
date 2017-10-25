package spgui.widgets.labkit

import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration._
import scala.scalajs.js.timers._
import sp.domain._
import sp.domain.Logic._

import scala.language.higherKinds
import scala.util.{Try, Success, Failure }

import fs2._
import fs2.async
import fs2.async.mutable.Queue
import scala.concurrent.ExecutionContext.Implicits.global
import cats.effect.{ Effect, IO }

class APIComm[RQT,RST](requestTopic: String, responseTopic: String, from: String, to: String,
  onChannelUp: Option[() => Unit], onMessage: Option[(SPHeader,RST) => Unit])
  (implicit val writeRequest: JSFormat[RQT], implicit val readResponse: JSFormat[RST]) {
  import spgui.communication.{BackendCommunication => bc }

  private class ManualSource {
    var _cb: Option[Either[Throwable, Option[(SPHeader, RST)]] => Unit] = None
    def registerPostFunction(cb: Either[Throwable, Option[(SPHeader, RST)]] => Unit): Unit = {
      _cb = Some(cb)
    }
    def post(header: SPHeader, body: RST): Unit = _cb.foreach(_(Right(Some((header,body)))))
    def done(): Unit = _cb.foreach(_(Right(None)))
    def error(err: Exception): Unit = _cb.foreach(_(Left(err)))
  }

  private def createStream(h: ManualSource): Stream[IO, Option[(SPHeader, RST)]] = {
    for {
      q <- Stream.eval(async.boundedQueue[IO, Either[Throwable, Option[(SPHeader, RST)]]](1))
      _ <- Stream.suspend {
        h.registerPostFunction { e => async.unsafeRunAsync(q.enqueue1(e))(_ => IO.unit) }; Stream.emit(())
      }
      row <- q.dequeue.rethrow
    } yield row
  }

  private var reqs: Map[ID, (ManualSource, SetTimeoutHandle)] = Map() // order of declaration important!

  private val topicHandler = bc.getMessageObserver(topicHandlerCB, responseTopic)
  private val channelObserver = bc.getWebSocketStatusObserver(up => if(up) onChannelUp.foreach(_.apply), responseTopic)

  private def topicHandlerCB(mess: SPMessage): Unit = {
    for {
      cb <- onMessage
      h <- mess.header.to[SPHeader].toOption
      b <- mess.body.to[RST](readResponse).toOption
    } cb.apply(h,b)

    for {
      h <- mess.header.to[SPHeader].toOption
      (source, timeout) <- reqs.get(h.reqID)
    } yield {
      mess.body.to[APISP].foreach {
        case APISP.SPACK() => // todo, create a stream of streams for all ACK:ing services?
        case APISP.SPError(message, _) => source.error(new RuntimeException(message))
        case APISP.SPDone() =>
          clearTimeout(timeout)
          source.done()
        case _ => // do nothing
      }

      mess.body.to[RST](readResponse).foreach(source.post(h,_))
    }
  }

  def ask(body: RQT): Stream[IO, (SPHeader,RST)] = ask(SPHeader(from = from, to = to), body)

  def ask(header: SPHeader, body: RQT, timeout: FiniteDuration = 1000 millis): Stream[IO, (SPHeader,RST)] = {
    val msg = SPMessage.make[SPHeader, RQT](header, body)(implicitly, writeRequest)
    bc.publish(msg, requestTopic)
    val source = new ManualSource

    val t = setTimeout(timeout) {
      reqs -= header.reqID
      val errMsg = s"No reply to request ${header.reqID} from service in ${timeout.toMillis}ms. (Check that the service posts a SPDone()"
      source.error(new java.util.concurrent.TimeoutException(errMsg))
    }

    reqs += (header.reqID -> (source, t))
    createStream(source).unNoneTerminate
  }

  // for conveniece, only take the first reply in the stream
  def ask1(body: RQT): Future[(SPHeader,RST)] = ask1(SPHeader(from = from, to = to), body)
  def ask1(header: SPHeader, body: RQT, timeout: FiniteDuration = 1000 millis): Future[(SPHeader,RST)] = {
    ask(header, body, timeout).runLog.unsafeToFuture().map(_.headOption).
      map(_.getOrElse(throw new RuntimeException("Service closed without providing any answers!")))
  }

  // for conveniece, when you only want to know that your request has been handled (e.g. save to a model)
  def ask0(body: RQT): Future[(SPHeader,RST)] = ask1(SPHeader(from = from, to = to), body)
  def ask0(header: SPHeader, body: RQT, timeout: FiniteDuration = 1000 millis): Future[Unit] = {
    ask(header, body, timeout).run.unsafeToFuture()
  }

  def tell(body: RQT): Unit =
    tell(SPHeader(from = from, to = to), body)

  def tell(header: SPHeader, body: RQT): Unit = {
    val msg = SPMessage.make[SPHeader, RQT](header, body)(implicitly, writeRequest)
    bc.publish(msg, requestTopic)
  }

  def kill = topicHandler.kill()
}
