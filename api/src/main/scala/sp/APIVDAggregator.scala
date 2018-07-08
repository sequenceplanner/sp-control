
package sp.VDAggregator {

  import sp.abilityhandler.APIAbilityHandler
  import sp.abilityhandler.APIAbilityHandler.Formats.fAbility
  import sp.devicehandler.VD.{Driver, DriverState, ResourceWithState}
  import sp.domain._
  import sp.domain.Logic._

  object APIVDAggregator {
    val topicRequest = "AggregatorRequest"
    val topicResponse = "AggregatorResponse"
    val service = "AggregatorService"

    sealed trait Request

    sealed trait Response


    case object GetDrivers extends Request
    case object GetResources extends Request
    case object GetAbilities extends Request


    case class driverInfo(driver: Driver, driverState: DriverState, status: String)

    case class TheDrivers(Drivers: List[driverInfo]) extends Response
    case class TheResources(resources: List[ResourceWithState]) extends Response
    case class TheAbilities(abilities: List[APIAbilityHandler.Ability]) extends Response
    case class TheAbilityStates(abilityStates: Map[ID, SPValue]) extends Response
    case class TheRunnerStates(runnerStates: Map[ID, Map[ID, SPValue]] ) extends Response

    object Formats {

      import play.api.libs.json._

      implicit lazy val fGetDrivers: JSFormat[GetDrivers.type] = deriveCaseObject[GetDrivers.type]
      implicit lazy val fGetResources: JSFormat[GetResources.type] = deriveCaseObject[GetResources.type]
      implicit lazy val fGetAbilities: JSFormat[GetAbilities.type] = deriveCaseObject[GetAbilities.type]


      implicit lazy val fdriverInfo: JSFormat[driverInfo] = Json.format[driverInfo]

      implicit lazy val fTheDrivers: JSFormat[TheDrivers] = Json.format[TheDrivers]
      implicit lazy val fTheResources: JSFormat[TheResources] = Json.format[TheResources]
      implicit lazy val fTheAbilities: JSFormat[TheAbilities] = Json.format[TheAbilities]
      implicit lazy val fTheAbilityStates: JSFormat[TheAbilityStates] = Json.format[TheAbilityStates]



      implicit lazy val mapMapReads: JSReads[Map[ID, Map[ID, SPValue]]] = new JSReads[Map[ID, Map[ID, SPValue]]] {
        override def reads(json: JsValue): JsResult[Map[ID, Map[ID, SPValue]]] = {
          json.validate[Map[String, SPValue]].map(xs =>
            xs.collect{case (k, v) if ID.isID(k)  => ( ID.makeID(k).get -> v.validate[Map[String, SPValue]].map(xs1 => xs1.collect{case (k1, v1) if ID.isID(k1)  => ( ID.makeID(k1).get -> v1) }).get)})
        }
      }
      implicit lazy val mapMapWrites: JSWrites[Map[ID, Map[ID, SPValue]]] = new OWrites[Map[ID, Map[ID, SPValue]]] {
        override def writes(xs: Map[ID, Map[ID, SPValue]]): JsObject = {
          val toFixedMap = xs.map{case (k, v) => k.toString -> SPValue(v.map{case (k1, v1) => k1.toString -> v1 }) }
          JsObject(toFixedMap)
        }
      }


      implicit lazy val fTheRunnerStates: JSFormat[TheRunnerStates] = Json.format[TheRunnerStates]

      def fDeviceDriverRequest: JSFormat[Request] = Json.format[Request]
      def fDeviceDriverResponse: JSFormat[Response] = Json.format[Response]
    }


    object Request {
      implicit lazy val fDeviceDriverRequest: JSFormat[Request] = Formats.fDeviceDriverRequest
    }

    object Response {
      implicit lazy val fDeviceDriverResponse: JSFormat[Response] = Formats.fDeviceDriverResponse
    }

  }
}