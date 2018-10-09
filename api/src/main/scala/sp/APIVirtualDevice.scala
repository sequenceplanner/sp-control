package sp.devicehandler {

  import sp.domain._
  import sp.domain.Logic._


  // shared
  object VD {
    type State = Map[ID, SPValue]
    type DriverState =  Map[String, SPValue]

    case class Resource(name: String, id: ID, things: Set[ID], stateMap: List[DriverStateMapper], setup: SPAttributes, sendOnlyDiffs: Boolean = false)
    case class Driver(name: String, id: ID, driverType: String, setup: SPAttributes)
    case class ResourceWithState(resource: Resource, state: State)
    case class DriverWithState(d: Driver, state: DriverState)



    sealed trait DriverStateMapper
    case class OneToOneMapper(thing: ID, driverID: ID, driverIdentifier: String) extends DriverStateMapper

    def resourceToThing(r: Resource): Thing = {
      Thing(
        name = r.name,
        id = r.id,
        attributes = SPAttributes(
          "things" -> r.things,
          "setup" -> r.setup,
          "sendOnlyDiffs" -> r.sendOnlyDiffs,
          "stateMap" -> r.stateMap
        )
      )
    }
    def driverToThing(d: Driver): Thing = {
      Thing(
        name = d.name,
        id = d.id,
        attributes = SPAttributes(
          "setup" -> d.setup,
          "driverType" -> d.driverType
        )
      )
    }
    def thingToResource(t: Thing): Resource = {
      val s = t.attributes.getAs[SPAttributes]("setup").getOrElse(SPAttributes())
      val only = t.attributes.getAs[Boolean]("sendOnlyDiffs").getOrElse(false)
      val sMap = t.attributes.getAs[List[DriverStateMapper]]("stateMap").getOrElse(List())
      val things = t.attributes.getAs[Set[ID]]("things").getOrElse(Set())
      Resource(t.name, t.id, things, sMap, s, only)
    }
    def thingToDriver(t: Thing): Driver = {
      val s = t.attributes.getAs[SPAttributes]("setup").getOrElse(SPAttributes())
      val dT = t.attributes.getAs[String]("driverType").getOrElse("")
      Driver(t.name, t.id, dT, s)
    }


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
    case class SetUpVD(name: String, id: ID, operations: List[Operation], resources: List[Resource], drivers: List[Driver], attributes: SPAttributes = SPAttributes()) extends Request
    case class SetUpVD2(name: String, id: ID, operations: List[Operation],
      resources: List[Resource], drivers: List[Driver], initialState: Map[ID, SPValue], model: List[IDAble] = List(),
      attributes: SPAttributes = SPAttributes()) extends Request

    // TODO: If needed, add a setup based on a struct and a model
    case class TerminateVD(id: ID) extends Request
    case object TerminateAllVDs extends Request
    case object TerminatedAllVDs extends Response
    case class TerminatedVD(id :ID) extends Response


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
      implicit lazy val fSetUpVD2:     JSFormat[SetUpVD2]     = Json.format[SetUpVD2]

      implicit lazy val fTerminateVD:     JSFormat[TerminateVD]     = Json.format[TerminateVD]
      implicit lazy val fTerminatedVD:     JSFormat[TerminatedVD]     = Json.format[TerminatedVD]
      implicit lazy val fTerminateAllVDs:     JSFormat[TerminateAllVDs.type ]     = deriveCaseObject[TerminateAllVDs.type]
      implicit lazy val fTerminatedAllVDs:     JSFormat[TerminatedAllVDs.type ]     = deriveCaseObject[TerminatedAllVDs.type]
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

    /**
      * A request to get the driver information and its current state.
      * Will respond with [[APIDeviceDriver.TheDriver]]
      */
    case object GetDriver extends Request

    case object GetDrivers extends Request


    /**
      * A request to set up a new driver instance. Sometimes this will also start the generic driver, but
      * sometimes you have to start the generic driver in a SP node.
      * Will respond with an [[APISP.SPDone]] or [[APISP.SPError]]
      * @param driver
      */
    case class SetUpDeviceDriver(driver: Driver) extends Request

    /**
      * A request to change the state of the driver
      * Will respond with [[APIDeviceDriver.DriverCommandDone]]
      * @param driverID The id of the driver
      * @param state The state to change. Only need to include the variables you would like to change
      */
    case class DriverCommand(driverID: ID, state: Map[String, SPValue]) extends Request

    /**
      * A request to terminate the driver instance.
      * will respond with [[APIDeviceDriver.DriverTerminated]]
      * @param id
      */
    case class TerminateDriver(id: ID) extends Request

    /**
      * The response from the driver that a command has been performed. The result is given in result
      * @param requestID The id of the request given in the header when [[APIDeviceDriver.DriverCommand]] was given.
      * @param successful A boolean if the command was successful or not.
      */
    case class DriverCommandDone(requestID: ID, successful: Boolean) extends Response

    /**
      * An event sent out by the driver when its state has changed.
      * @param name The name of the driver
      * @param id The ID of the driver
      * @param state The new state of the driver
      * @param diff a boolean variable saying if the given state only include the variables that changed. If true,
      *             the complete state is retrieved by the GetDriver request
      */
    case class DriverStateChange(name: String, id: ID, state: Map[String, SPValue], diff: Boolean = false) extends Response

    /**
      * A response including the driver information and its current state
      * @param x The Driver defined as [[VD.Driver]]
      * @param driverState The driver state defined as a Map[String, SPValue]
      */
    case class TheDriver(x: Driver, driverState: DriverState) extends Response

    case class TheDrivers(Drivers: List[(Driver, DriverState, String)]) extends Response

    /**
      * A response that the driver has been terminated. The message is not guranteed to come, for example
      * if the driver crashes.
      * @param id
      */
    case class DriverTerminated(id: ID) extends Response

    object Formats {
      import play.api.libs.json._
      implicit lazy val fGetDriver:     JSFormat[GetDriver.type]     = deriveCaseObject[GetDriver.type]
      implicit lazy val fGetDrivers:     JSFormat[GetDrivers.type]     = deriveCaseObject[GetDrivers.type]
      implicit lazy val fSetUpDeviceDriver: JSFormat[SetUpDeviceDriver] = Json.format[SetUpDeviceDriver]
      implicit lazy val fDriverStateChange:     JSFormat[DriverStateChange]     = Json.format[DriverStateChange]
      implicit lazy val fDriverCommand:     JSFormat[DriverCommand]     = Json.format[DriverCommand]
      implicit lazy val fDriverCommandDone:     JSFormat[DriverCommandDone]     = Json.format[DriverCommandDone]
      implicit lazy val fTheDriver:     JSFormat[TheDriver]     = Json.format[TheDriver]
      implicit lazy val fTheDrivers:     JSFormat[TheDrivers]     = Json.format[TheDrivers]
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
