package sp.virtcom

import play.api.libs.json._
import sp.domain._
import sp.domain.logic.JsonImplicit
import sp.domain.Logic._

object APIVolvoScheduler {
  sealed trait Request
  sealed trait Response
  val service = "VolvoScheduler"
  val topicRequest = "VolvoSchedulerRequests"
  val topicResponse = "VolvoSchedulerResponse"

  case class generateSOPs(modelID : ID, schedules : Set[ID], ids : List[IDAble] ) extends Request
  case class generatedSopID(SopId :ID) extends Response
  case class calculateStructID(StructId :ID) extends Response
  case class cpResults(cpRes : SPAttributes) extends Response

  case class getCases(SopID : ID, ids : List[IDAble]) extends  Request
  case class gotCases(map: Map[String, List[Operation]]) extends Response
  case class calculate(modelID : ID, SopID : ID, ids : List[IDAble], neglectedCases : Set[ID]) extends Request


  object Formats {
    implicit val fgenerateSOPs: JSFormat[generateSOPs] = Json.format[generateSOPs]
    implicit val fgeneratedSopID: JSFormat[generatedSopID] = Json.format[generatedSopID]
    implicit val fcalculateStructID: JSFormat[calculateStructID] = Json.format[calculateStructID]
    implicit val fcpResults: JSFormat[cpResults] = Json.format[cpResults]
    implicit val fgetCases: JSFormat[getCases] = Json.format[getCases]
    implicit val fgotCases: JSFormat[gotCases] = Json.format[gotCases]
    implicit val fcalculate: JSFormat[calculate] = Json.format[calculate]

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