
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

    object Formats {

      import play.api.libs.json._

      implicit lazy val fGetDrivers: JSFormat[GetDrivers.type] = deriveCaseObject[GetDrivers.type]
      implicit lazy val fGetResources: JSFormat[GetResources.type] = deriveCaseObject[GetResources.type]
      implicit lazy val fGetAbilities: JSFormat[GetAbilities.type] = deriveCaseObject[GetAbilities.type]


      implicit lazy val fdriverInfo: JSFormat[driverInfo] = Json.format[driverInfo]

      implicit lazy val fTheDrivers: JSFormat[TheDrivers] = Json.format[TheDrivers]
      implicit lazy val fTheResources: JSFormat[TheResources] = Json.format[TheResources]
      implicit lazy val fTheAbilities: JSFormat[TheAbilities] = Json.format[TheAbilities]

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