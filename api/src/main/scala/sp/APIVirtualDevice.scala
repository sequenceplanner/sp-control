package sp.devicehandler {

  import sp.domain._
  import sp.domain.Logic._


  // shared
  object VD {
    type State = Map[ID, SPValue]
    type DriverState =  Map[String, SPValue]

    case class Resource(name: String, id: ID, things: Set[ID], stateMap: List[DriverStateMapper], setup: SPAttributes, sendOnlyDiffs: Boolean = false)
    case class Driver(name: String, id: ID, driverType: String, setup: SPAttributes)
    case class ResourceWithState(r: Resource, state: State)
    case class DriverWithState(d: Driver, state: DriverState)

    sealed trait DriverStateMapper
    case class OneToOneMapper(thing: ID, driverID: ID, driverIdentifier: String) extends DriverStateMapper


    // Json
    import play.api.libs.json._
    object Resource {
      implicit lazy val fResource: JSFormat[Resource] = Json.format[Resource]
    }
    object Driver {
      implicit lazy val fDriver: JSFormat[Driver] = Json.format[Driver]
    }
    object OneToOneMapper {
      implicit lazy val fOneToOneMapper: JSFormat[OneToOneMapper] = Json.format[OneToOneMapper]
    }
    object DriverStateMapper {
      implicit lazy val fDriverStateMapper: JSFormat[DriverStateMapper] = Json.format[DriverStateMapper]
    }
    object ResourceWithState {
      implicit lazy val fResourceWithState: JSFormat[ResourceWithState] = Json.format[ResourceWithState]
    }
    object DriverWithState {
      implicit lazy val fDriverWithState: JSFormat[DriverWithState] = Json.format[DriverWithState]
    }
  }



  object APIVirtualDevice {
    val service = "VirtualDevice"
    val topicRequest = "virtualDeviceRequests"
    val topicResponse = "virtualDeviceResponse"

    sealed trait Request
    sealed trait Response

    import VD._

    // requests setup. Include the model id if it exist in the attributes
    case class SetUpVD(name: String, id: ID, resources: List[Resource], drivers: List[Driver], attributes: SPAttributes = SPAttributes()) extends Request
    // TODO: If needed, add a setup based on a struct and a model
    case class TerminateVD(id: ID) extends Request
    case object GetVD extends Request
    // requests command (gets a SPACK and when applied, SPDone (and indirectly a StateEvent))
    case class VDCommand(resource: ID, stateRequest: Map[ID, SPValue], timeout: Int = 0) extends Request

    // TODO: Add below when needed
    //case class AddResource(resource: Resource) extends Request
    //case class AddDriver(driver: Driver) extends Request
    //case class RemoveResource(id: ID) extends Request
    //case class RemoveDriver(id: ID) extends Request


    // answers
    case class TheVD(name: String,
                     id: ID,
                     resources: List[ResourceWithState],
                     drivers: List[DriverWithState],
                     attributes: SPAttributes
                    ) extends Response

    // TODO: Probably only use the resource id, not the name...
    case class StateEvent(resource: String, id: ID, state: Map[ID, SPValue], diff: Boolean = false) extends Response

    // TODO: Add when needed
    //case class NewResource(x: Resource) extends Response
    //case class RemovedResource(id: ID) extends Response
    //case class NewDriver(x: Driver) extends Response
    //case class RemovedDriver(id: ID) extends Response


    object Formats {
      import play.api.libs.json._
      implicit lazy val fSetUpVD:     JSFormat[SetUpVD]     = Json.format[SetUpVD]
      implicit lazy val fTerminateVD:     JSFormat[TerminateVD]     = Json.format[TerminateVD]
      implicit lazy val fGetVD:     JSFormat[GetVD.type]     = deriveCaseObject[GetVD.type]
      implicit lazy val fTheVD:     JSFormat[TheVD]     = Json.format[TheVD]
      implicit lazy val fVDCommand:     JSFormat[VDCommand]     = Json.format[VDCommand]
      implicit lazy val fStateEvent:     JSFormat[StateEvent]     = Json.format[StateEvent]
      def fVirtualDeviceRequest: JSFormat[Request] = Json.format[Request]
      def fVirtualDeviceResponse: JSFormat[Response] = Json.format[Response]
    }


    object Request {
      implicit lazy val fVirtualDeviceRequest: JSFormat[Request] = Formats.fVirtualDeviceRequest
    }

    object Response {
      implicit lazy val fVirtualDeviceResponse: JSFormat[Response] = Formats.fVirtualDeviceResponse
    }


  }


  /**
    * The driver API is only used by the virtual device and the comm with drivers
    */
  object APIDeviceDriver {
    val topicRequest = "driverCommands"
    val topicResponse = "driverEvents"

    sealed trait Request
    sealed trait Response

    import VD._

    case object GetDriver extends Request
    case class SetUpDeviceDriver(driver: Driver) extends Request
    case class DriverCommand(driverID: ID, state: Map[String, SPValue]) extends Request
    case class TerminateDriver(id: ID) extends Request

    case class DriverCommandDone(requestID: ID, result: Boolean) extends Response
    case class DriverStateChange(name: String, id: ID, state: Map[String, SPValue], diff: Boolean = false) extends Response
    case class TheDriver(x: Driver, driverState: DriverState) extends Response
    case class DriverTerminated(id: ID) extends Response

    object Formats {
      import play.api.libs.json._
      implicit lazy val fGetDriver:     JSFormat[GetDriver.type]     = deriveCaseObject[GetDriver.type]
      implicit lazy val fSetUpDeviceDriver: JSFormat[SetUpDeviceDriver] = Json.format[SetUpDeviceDriver]
      implicit lazy val fDriverStateChange:     JSFormat[DriverStateChange]     = Json.format[DriverStateChange]
      implicit lazy val fDriverCommand:     JSFormat[DriverCommand]     = Json.format[DriverCommand]
      implicit lazy val fDriverCommandDone:     JSFormat[DriverCommandDone]     = Json.format[DriverCommandDone]
      implicit lazy val fTheDriver:     JSFormat[TheDriver]     = Json.format[TheDriver]
      implicit lazy val fDriverTerminated:     JSFormat[DriverTerminated]     = Json.format[DriverTerminated]
      implicit lazy val fTerminateDriver:     JSFormat[TerminateDriver]     = Json.format[TerminateDriver]
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


