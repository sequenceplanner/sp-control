package sp.unification

import play.api.libs.json._
import sp.abilityhandler.APIAbilityHandler
import sp.devicehandler.VD
import sp.domain._
import sp.domain.logic.JsonImplicit
import sp.domain.Logic._
import sp.runners.APIOperationRunner

case class ResourceAndAbilities(abilities: List[APIAbilityHandler.Ability],
                                resource: VD.Resource,
                                driver: VD.Driver,
                                vars: List[Thing],
                                moveToID: ID,
                                refID: ID,
                                currentID: ID,
                                activateID: ID
                               )

object APIUnification {
  sealed trait Request
  sealed trait Response
  val service = "Unification"
  val topicRequest = "UnificationRequests"
  val topicResponse = "UnificationResponse"

  case class createModel(id :ID = ID.newID) extends  Request
  case class launchVDAbilities(idables : List[IDAble]) extends  Request
  case class launchOpRunner(idables : List[IDAble]) extends  Request

  case class getUnificationModelsInfo(dummy : String = "") extends Request
  case class getUnificationModel(modelName : String) extends Request

  case class sendUnificationModelInfo(modelName : String, modelTags : List[String] = List()) extends Response
  case class sendUnificationModel(modelName : String, resources : List[ResourceAndAbilities], setupRunner : APIOperationRunner.CreateRunner) extends Request




  object Formats {
    import sp.runners.APIOperationRunner.Formats.fCreateRunner
    import sp.abilityhandler.APIAbilityHandler.Formats.fAbility
    import sp.devicehandler.VD.Resource.fResource
    import sp.devicehandler.VD.Driver.fDriver

    implicit val fcreateModel: JSFormat[createModel] = Json.format[createModel]
    implicit val flaunchVDAbilities: JSFormat[launchVDAbilities] = Json.format[launchVDAbilities]
    implicit val flaunchOpRunner: JSFormat[launchOpRunner] = Json.format[launchOpRunner]

    implicit val fgetUnificationModelsInfo: JSFormat[getUnificationModelsInfo] = Json.format[getUnificationModelsInfo]
    implicit val fgetUnificationModel: JSFormat[getUnificationModel] = Json.format[getUnificationModel]
    implicit val fsendUnificationModelInfo: JSFormat[sendUnificationModelInfo] = Json.format[sendUnificationModelInfo]

    implicit val fResourceAndAbilities: JSFormat[ResourceAndAbilities] = Json.format[ResourceAndAbilities]

    implicit val fsendUnificationModel: JSFormat[sendUnificationModel] = Json.format[sendUnificationModel]

    def fUnificationServiceRequest: JSFormat[Request] = Json.format[Request]
    def fUnificationServiceResponse: JSFormat[Response] = Json.format[Response]
  }

  object Request {
    implicit lazy val fUnificationServiceRequest: JSFormat[Request] = Formats.fUnificationServiceRequest
  }

  object Response {
    implicit lazy val fUnificationServiceResponse: JSFormat[Response] = Formats.fUnificationServiceResponse
  }
}
