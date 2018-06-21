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

  case class getModelsInfo(dummy : String = "") extends Request
  case class sendModelsInfo(models : List[String] = List()) extends Response

  case class createModel(modelType: String, id :ID = ID.newID) extends  Request
  case class launchVDAbilities(idables : List[IDAble]) extends  Request
  case class launchOpRunner(idables : List[IDAble]) extends  Request
  case class OpRunnerCreated(id: ID) extends Response

  case object ResetGUI extends Request

  object Formats {
    implicit val fResetGUI: JSFormat[ResetGUI.type ] = deriveCaseObject[ResetGUI.type ]

    implicit val fcreateModel: JSFormat[createModel] = Json.format[createModel]
    implicit val flaunchVDAbilities: JSFormat[launchVDAbilities] = Json.format[launchVDAbilities]
    implicit val flaunchOpRunner: JSFormat[launchOpRunner] = Json.format[launchOpRunner]
    implicit val fOpRunnerCreated: JSFormat[OpRunnerCreated] = Json.format[OpRunnerCreated]

    implicit val fgetModelsInfo: JSFormat[getModelsInfo] = Json.format[getModelsInfo]
    implicit val fsendModelInfo: JSFormat[sendModelsInfo] = Json.format[sendModelsInfo]


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
