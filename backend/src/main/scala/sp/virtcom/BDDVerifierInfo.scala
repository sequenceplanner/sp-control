package sp.virtcom

import sp.domain._
import sp.virtcom.{APIBDDVerifier => api}

object BDDVerifierInfo {
    import sp.domain.SchemaLogic._
    case class BDDVerifierRequest(request: api.Request)
    case class BDDVerifierResponse(response: api.Response)

    lazy val req: com.sksamuel.avro4s.SchemaFor[BDDVerifierRequest] = com.sksamuel.avro4s.SchemaFor[BDDVerifierRequest]
   // lazy val resp: com.sksamuel.avro4s.SchemaFor[BDDVerifierResponse] = com.sksamuel.avro4s.SchemaFor[BDDVerifierResponse]

/*    val apischema = makeMeASchema(
      req(),
      resp()
    ) */

    val attributes: APISP.StatusResponse = APISP.StatusResponse(
      service = api.service,
      tags = List("BDDVerifier"),
    //  api = apischema,
      version = 1,
      topicRequest = api.topicRequest,
      topicResponse = api.topicResponse,
      attributes = SPAttributes.empty
    )
  }
