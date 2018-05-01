package sp.unification

import sp.domain._
import sp.unification.{APIUnification => api}

object UnificationInfo {
    import sp.domain.SchemaLogic._
    case class UnificationRequest(request: api.Request)
    case class UnificationResponse(response: api.Response)

    lazy val req: com.sksamuel.avro4s.SchemaFor[UnificationRequest] = com.sksamuel.avro4s.SchemaFor[UnificationRequest]
    lazy val resp: com.sksamuel.avro4s.SchemaFor[UnificationResponse] = com.sksamuel.avro4s.SchemaFor[UnificationResponse]

    val apischema = makeMeASchema(
      req(),
      resp()
    )

    val attributes: APISP.StatusResponse = APISP.StatusResponse(
      service = api.service,
      tags = List("Unification"),
      api = apischema,
      version = 1,
      topicRequest = api.topicRequest,
      topicResponse = api.topicResponse,
      attributes = SPAttributes.empty
    )
  }
