package sp.modelImport
import play.api.libs.json._
import sp.domain.logic.JsonImplicit
import sp.domain.Logic._
import sp.domain._

object APISPModelImport {
  sealed trait Request
  sealed trait Response
  val service = "SPModelImport"
  val topicRequest = "SPModelImportRequests"
  val topicResponse = "SPModelImportResponse"

  case class ImportText(Text: String) extends Request // The model arrives as a text string.
  case class ModelInfo(id: ID, name: String, version: Long, attributes: SPAttributes, history: List[SPAttributes])
  case class Model(model: ID, info: ModelInfo, ids: List[IDAble]) extends Response // This is the expected format of the old models


  object Formats {
    implicit val fImportText: JSFormat[ImportText] = Json.format[ImportText]
    implicit val fModelInfo: JSFormat[ModelInfo] = Json.format[ModelInfo]
    implicit val fModel: JSFormat[Model] = Json.format[Model]

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