package sp.modelSupport

import play.api.libs.json._
import sp.domain._
import sp.domain.logic.JsonImplicit
import sp.domain.Logic._

object APIMiniModelService {
  sealed trait Request
  sealed trait Response
  val service = "MiniModelHelper"
  val topicRequest = service+"Request"
  val topicResponse = service+"Response"

  case object getModelsInfo extends Request
  case class sendModelsInfo(models : List[String] = List()) extends Response

  case class createModel(modelType: String, id :ID = ID.newID) extends  Request


  case class bmc(model: List[IDAble], initialState: Map[ID, SPValue], query: String, bound: Int = 50) extends  Request
  case class bmcOutput(stdout: String) extends  Response



  object Formats {
    implicit val fcreateModel: JSFormat[createModel] = Json.format[createModel]
    implicit val fgetModelsInfo: JSFormat[getModelsInfo.type] = deriveCaseObject[getModelsInfo.type]
    implicit val fsendModelInfo: JSFormat[sendModelsInfo] = Json.format[sendModelsInfo]
    implicit val fbmc: JSFormat[bmc] = Json.format[bmc]
    implicit val fbmcOutput: JSFormat[bmcOutput] = Json.format[bmcOutput]

    def fMiniModelServiceRequest: JSFormat[Request] = Json.format[Request]
    def fMiniModelServiceResponse: JSFormat[Response] = Json.format[Response]
  }

  object Request {
    implicit lazy val fExampleServiceRequest: JSFormat[Request] = Formats.fMiniModelServiceRequest
  }

  object Response {
    implicit lazy val fExampleServiceResponse: JSFormat[Response] = Formats.fMiniModelServiceResponse
  }
}
