package sp.modelImport

import sp.domain._
import sp.modelImport.{APISPModelImport =>api}

object SPModelImportInfo {
    import sp.domain.SchemaLogic._
    case class SPModelImportRequest(request: api.Request)
    case class SPModelImportResponse(response: api.Response)

    lazy val req: com.sksamuel.avro4s.SchemaFor[SPModelImportRequest] = com.sksamuel.avro4s.SchemaFor[SPModelImportRequest]
    lazy val resp: com.sksamuel.avro4s.SchemaFor[SPModelImportResponse] = com.sksamuel.avro4s.SchemaFor[SPModelImportResponse]

    val apischema = makeMeASchema(
      req(),
      resp()
    )

    val attributes: APISP.StatusResponse = APISP.StatusResponse(
      service = api.service,
      tags = List("modelImport"),
      api = apischema,
      version = 1,
      topicRequest = api.topicRequest,
      topicResponse = api.topicResponse,
      attributes = SPAttributes.empty
    )
  }
