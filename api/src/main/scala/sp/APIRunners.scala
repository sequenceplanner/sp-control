package sp.runners {

  import sp.domain._
  import sp.domain.Logic._

  object Shared {
    type State = Map[ID, SPValue]
    object State {
      def empty = Map[ID, SPValue]()
    }

    // move resources here when they are serializable
  }

  object APIRunnerManager {
    val service = "RunnerManager"
    val topicRequest = "RunnerManagerRequests"
    val topicResponse = "RunnerManagerResponse"

    sealed trait Request
    sealed trait Response

    case class RunnerStarted(id: ID) extends Response

    case class TerminateRunnerInstance(id: ID) extends Request
    case object TerminateAllRunnerInstances extends Request
    case class TerminatedRunnerInstance(id :ID) extends Response
    case object TerminatedAllRunnerInstances extends Response

    case class StateEvent(runnerInstance: ID, state: Map[ID, SPValue]) extends Response
    case class TakeStep(runnerInstance: ID) extends Request
    case class SetStepping(step: Boolean, instanceID: ID) extends Request
    case class StopAuto(runnerInstance: ID) extends Request
    case class StartAuto(runnerInstance: ID) extends Request
    case class SetForceTable(runnerInstance: ID, force: Map[ID, SPValue], events: Map[ID, SPValue]) extends Request
    case class SetPlan(runnerInstance: ID, plan: List[String]) extends Request

    object Formats {
      import play.api.libs.json._
      implicit lazy val fRunnerStarted:     JSFormat[RunnerStarted]     = Json.format[RunnerStarted]
      implicit lazy val fTerminateVD:     JSFormat[TerminateRunnerInstance]     = Json.format[TerminateRunnerInstance]
      implicit lazy val fTerminatedVD:     JSFormat[TerminatedRunnerInstance]     = Json.format[TerminatedRunnerInstance]
      implicit lazy val fTerminateAllVDs:     JSFormat[TerminateAllRunnerInstances.type ]     = deriveCaseObject[TerminateAllRunnerInstances.type]
      implicit lazy val fTerminatedAllVDs:     JSFormat[TerminatedAllRunnerInstances.type]     = deriveCaseObject[TerminatedAllRunnerInstances.type]
      implicit lazy val fStateEvent:     JSFormat[StateEvent]     = Json.format[StateEvent]
      implicit lazy val fTakeStep:     JSFormat[TakeStep]     = Json.format[TakeStep]
      implicit lazy val fSetStepping:     JSFormat[SetStepping]     = Json.format[SetStepping]
      implicit lazy val fStopAuto:     JSFormat[StopAuto]     = Json.format[StopAuto]
      implicit lazy val fStartAuto:     JSFormat[StartAuto]     = Json.format[StartAuto]
      implicit lazy val fSetForceTable:     JSFormat[SetForceTable]     = Json.format[SetForceTable]
      implicit lazy val fSetPlan:     JSFormat[SetPlan]     = Json.format[SetPlan]


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
}
