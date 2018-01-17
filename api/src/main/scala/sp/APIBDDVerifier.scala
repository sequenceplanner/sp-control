package sp.virtcom

import play.api.libs.json._
import sp.domain.{JSFormat, _}
import sp.domain.logic.JsonImplicit
import sp.domain.Logic._

object APIBDDVerifier {
  sealed trait Request
  sealed trait Response
  val service = "BDDVerifier"
  val topicRequest = "BDDVerifierRequests"
  val topicResponse = "BDDVerifierResponse"

  case class VerifyBDD(bddName :String, partialState : Map[String, Int]) extends  Request
  case class VerificationResult(res : Boolean) extends Response

  object Formats {
    implicit val fVerifyBDD: JSFormat[VerifyBDD] = Json.format[VerifyBDD]
    implicit val fVerificationResult: JSFormat[VerificationResult] = Json.format[VerificationResult]

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