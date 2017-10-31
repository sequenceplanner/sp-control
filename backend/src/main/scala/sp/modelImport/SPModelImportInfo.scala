package sp.modelImport

import sp.domain._
import sp.models.APIModel

object SPModelImportInfo {
    import sp.domain.SchemaLogic._
    case class ExampleServiceRequest(request: APIModel.Request)
    case class ExampleServiceResponse(response: APIModel.Response)

    lazy val req: com.sksamuel.avro4s.SchemaFor[ExampleServiceRequest] = com.sksamuel.avro4s.SchemaFor[ExampleServiceRequest]
    lazy val resp: com.sksamuel.avro4s.SchemaFor[ExampleServiceResponse] = com.sksamuel.avro4s.SchemaFor[ExampleServiceResponse]

    val apischema = makeMeASchema(
      req(),
      resp()
    )

    val attributes: APISP.StatusResponse = APISP.StatusResponse(
      service = APIModel.service,
      tags = List("virtcom"),
      api = apischema,
      version = 1,
      topicRequest = APIModel.topicRequest,
      topicResponse = APIModel.topicResponse,
      attributes = SPAttributes.empty
    )
  }
