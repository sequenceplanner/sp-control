package sp.drivers.ros2

import play.api.libs.json._
import sp.domain.{JSFormat, _}
import sp.domain.logic.JsonImplicit
import sp.domain.Logic._

object APIRosFrontendHelper {
  sealed trait Request
  sealed trait Response
  val service = "RosFrontendHelper"
  val topicRequest = "RosFrontendHelperRequest"
  val topicResponse = "RosFrontendHelperResponse"

  case class GetEmptyMessage(msgType: String) extends Request
  case class Publish(msgType: String, topic: String, msg: SPAttributes) extends Request
  case class Subscribe(msgType: String, topic: String) extends Request
  case object StopSubscribe extends Request


  case class EmptyMessage(msg: Option[SPAttributes]) extends Response
  case class RosMessage(topic: String, msg: SPAttributes) extends Response

  object Formats {
    implicit val fGetEmpty: JSFormat[GetEmptyMessage] = Json.format[GetEmptyMessage]
    implicit val fPublish: JSFormat[Publish] = Json.format[Publish]
    implicit val fSubscribe: JSFormat[Subscribe] = Json.format[Subscribe]
    implicit val fStopSubscribe:     JSFormat[StopSubscribe.type]     = deriveCaseObject[StopSubscribe.type]
    implicit val fEmptyMessage: JSFormat[EmptyMessage] = Json.format[EmptyMessage]
    implicit val fRosMessage: JSFormat[RosMessage] = Json.format[RosMessage]

    def fRequest: JSFormat[Request] = Json.format[Request]
    def fResponse: JSFormat[Response] = Json.format[Response]
  }

  object Request {
    implicit lazy val fRequest: JSFormat[Request] = Formats.fRequest
  }

  object Response {
    implicit lazy val fResponse: JSFormat[Response] = Formats.fResponse
  }

}
