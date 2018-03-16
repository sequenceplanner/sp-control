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

  case class createModel(id :ID = ID.newID) extends  Request
  case class launchVD(id :ID, idables : List[IDAble]) extends  Request


  object Formats {
    implicit val fcreateModel: JSFormat[createModel] = Json.format[createModel]
    implicit val flaunchVD: JSFormat[launchVD] = Json.format[launchVD]

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
