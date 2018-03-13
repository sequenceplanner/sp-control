package sp.vdtesting

import play.api.libs.json._
import sp.domain._
import sp.domain.logic.JsonImplicit
import sp.domain.Logic._

object APIVDTracker {
  sealed trait Request
  sealed trait Response
  val service = "VDTracker"
  val topicRequest = "VDTrackerRequests"
  val topicResponse = "VDTrackerResponse"

  case class helloRequest( requestMessage:String) extends Request
  case class helloResponse( responseMessage: String) extends Response


  object Formats {
    implicit val fHelloRequest: JSFormat[helloRequest] = Json.format[helloRequest]
    implicit val fHelloResponse: JSFormat[helloResponse] = Json.format[helloResponse]

    def fExampleServiceRequest: JSFormat[Request] = Json.format[Request]
    def fExampleServiceResponse: JSFormat[Response] = Json.format[Response]
  }

  object Request {
    implicit lazy val fExampleServiceRequest: JSFormat[Request] = Formats.fExampleServiceRequest
  }

  object Response {
    implicit lazy val fExampleServiceResponse: JSFormat[Response] = Formats.fExampleServiceResponse
  }
}
