package sp.patrikmodel {

  import sp.domain._
  import sp.domain.Logic._

  object API {
    val service = "PatrikModel"
    val topicRequest = service+"Requests"
    val topicResponse = service+"Response"

    sealed trait Request
    sealed trait Response

    case object GetAvailableModels extends Request
    case class AvailableModels(models: List[String]) extends Response

    case class CreateManualModel(name: String) extends Request
    case class ManualModel(ids: List[IDAble]) extends Response

    object Formats {
      import play.api.libs.json._
      implicit lazy val fGetAvailableModels: JSFormat[GetAvailableModels.type] = deriveCaseObject[GetAvailableModels.type]
      implicit lazy val fAvailableModels: JSFormat[AvailableModels] = Json.format[AvailableModels]
      implicit lazy val fCreateManualModel: JSFormat[CreateManualModel] = Json.format[CreateManualModel]
      implicit lazy val fManualModel: JSFormat[ManualModel] = Json.format[ManualModel]

      def fPMRequest: JSFormat[Request] = Json.format[Request]
      def fPMResponse: JSFormat[Response] = Json.format[Response]
    }

    object Request {
      implicit lazy val fPMRquest: JSFormat[Request] = Formats.fPMRequest
    }

    object Response {
      implicit lazy val fPMResponse: JSFormat[Response] = Formats.fPMResponse
    }

  }


}
