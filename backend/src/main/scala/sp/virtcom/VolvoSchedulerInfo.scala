package sp.virtcom

import sp.domain._
import sp.modelImport.{APISPModelImport => api}

object VolvoSchedulerInfo {
    import sp.domain.SchemaLogic._
    case class VolvoSchedulerRequest(request: api.Request)
    case class VolvoSchedulerResponse(response: api.Response)

    lazy val req: com.sksamuel.avro4s.SchemaFor[VolvoSchedulerRequest] = com.sksamuel.avro4s.SchemaFor[VolvoSchedulerRequest]
    lazy val resp: com.sksamuel.avro4s.SchemaFor[VolvoSchedulerResponse] = com.sksamuel.avro4s.SchemaFor[VolvoSchedulerResponse]

    val apischema = makeMeASchema(
      req(),
      resp()
    )

    val attributes: APISP.StatusResponse = APISP.StatusResponse(
      service = api.service,
      tags = List("VolvoScheduler"),
      api = apischema,
      version = 1,
      topicRequest = api.topicRequest,
      topicResponse = api.topicResponse,
      attributes = SPAttributes.empty
    )
  }
