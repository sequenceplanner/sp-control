package sp.robotservices

import java.time._

import sp.domain.JSFormat

import sp.domain.Logic.deriveCaseObject

object APIRobotServices{

  sealed trait Request
  sealed trait Message
  sealed trait Response
  val vdService = "VDservice"
  val instructionFillerService = "Instruction Filler"
  val logPlayer = "Log Player"
  val topic = "Lisa"
  val topicRequest = "LisaRequest"
  val topicResponse = "LisaResponse"


  val activeMQUrl = "localhost"
  val activeMQPort = 61616
  val activeMQTopic = "LISA"
  val activeMQRequestTopic = "LISArequest"
  val routinesToIgnore: List[String] = List("testRout1", "testRout2")
  val homePosSignals: List[String] = List("O_Homepos", "R2UT_HomeAboveBP", "R4UT_HomePosLeft", "R5UT_HomePos")

//frontend
  case class LoadLog(path:String = "/home/ashfaqf/Projects/Lisa files/from_volvo/logs/log-13_12_35") extends Request
  case class LoadRobotModules(folderPath: String = "/home/ashfaqf/Projects/Lisa files/from_volvo/logs/robotProgs/w1741060") extends Request
  case object PlayLogs extends Request
  case object Connect extends Request
  case object StopPlayingLogs extends Request
  case class LoadWorkCells(path:String) extends Request
  
  case object Finished extends Response
  case object Started extends Response

  case class requestModules(robotId: String) extends Request
  case object requestWorkCellList extends Request
  case object EmptyMessage extends Message

  case class RobotDataAddress(domain: String,
                              kind: String,
                              path: List[String])


  // Activities
  case class Activity(id: String,
                      from: ZonedDateTime,
                      name: String,
                      to: ZonedDateTime,
                      `type`: String) extends Message

  case class ActivityEvent(activityId: String,
                           isStart: Boolean,
                           name: String,
                           robotId: String,
                           time: ZonedDateTime,
                           `type`: String,
                           workCellId: String) extends Message

  case class ActivityEventWithRobotCycle(activityId: String,
                           cycleId:String,
                           isStart: Boolean,
                           name: String,
                           robotId: String,
                           time: ZonedDateTime,
                           `type`: String,
                           workCellId: String) extends Message



  case class IncomingCycleEvent(address: SignalAddress,
                                newSignalState: NewSignalState,
                                robotId: String,
                                time: ZonedDateTime,
                                workCellId: String) extends Message

  case class OutgoingCycleEvent(cycleId: String,
                                isStart: Boolean,
                                time: ZonedDateTime,
                                workCellId: String) extends Message

  // Get work cells from endpoint
  case class WorkCell(id: String,
                      description: String,
                      robots: List[Robot])

  case class WorkCellList(workcells: List[WorkCell]) extends Message

  case class Robot(id: String,
                   name: String)

  // Cycle Fold, Store and Search
  case class WorkCellCycle(workCellId: String,
                           id: String,
                           from: ZonedDateTime,
                           to: ZonedDateTime,
                           activities: Map[String, Map[String, List[Activity]]]) extends Message

  case class WorkCellActivity(workCellId: String,
                              cycleId: String,
                              cycleStart: ZonedDateTime,
                              cycleEnd: ZonedDateTime,
                              resource: String,
                              activityId: String,
                              activityStart: ZonedDateTime,
                              activityEnd: ZonedDateTime,
                              name: String,
                              `type`: String) extends Message

  case class TimeSpan(from: ZonedDateTime,
                      to: ZonedDateTime)

  case class RobotCyclesResponse(workCellId: String,
                                 error: Option[String],
                                 foundCycles: Option[List[WorkCellCycle]]) extends Message

  // Robot Endpoint
  case class RapidAddress(domain: String,
                          kind: String,
                          path: Option[List[String]])

  case class SignalAddress(domain: String,
                           signal: String)

  // IO Signals
  case class NewSignalState(value: Float,
                            simulated: Boolean,
                            quality: Map[String, Int])

  // Program Pointer
  case class PointerChangedEvent(robotId: String,
                                 workCellId: String,
                                 address: RapidAddress,
                                 programPointerPosition: PointerPosition) extends Message

  case class PointerPosition(position: Position,
                             task: String,
                             time: ZonedDateTime)

  case class Position(module: String,
                      routine: String,
                      range: Range)

  case class Range(begin: Location,
                   end: Location)

  case class Location(column: Int,
                      row: Int)

  // RAPID Modules
  case class Module(name: String,
                    file: Option[List[String]])

  case class TaskWithModules(name: String,
                             modules: List[Module])


  case class ModulesReadEvent(robotId: String,
                              workCellId: String,
                              address: RapidAddress,
                              readValue: List[TaskWithModules]) extends Message




  // Instruction Fill
  case class PointerWithInstruction(robotId: String,
                                    workCellId: String,
                                    address: RapidAddress,
                                    instruction: String,
                                    programPointerPosition: PointerPosition) extends Message

  // Is Waiting Fill
  case class PointerWithIsWaiting(robotId: String,
                                  workCellId: String,
                                  address: RapidAddress,
                                  instruction: String,
                                  isWaiting: Boolean,
                                  programPointerPosition: PointerPosition) extends Message


  object Formats {
    import play.api.libs.json._
    import sp.domain._
    import play.api.libs.functional.syntax._
    //implicit val yourJodaDateReads = Reads.jodaDateReads("yyyy-MM-dd'T'HH:mm:ss'Z'")
    //implicit val yourJodaDateWrites = Writes.jodaDateWrites("yyyy-MM-dd'T'HH:mm:ss'Z")

    lazy val dateF = format.DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSz")
    implicit lazy val javatimeF = new Format[ZonedDateTime] {
      override def reads(json: JsValue): JsResult[ZonedDateTime] = {
        json.validate[String].map(ZonedDateTime.parse(_, dateF))
      }

      override def writes(o: ZonedDateTime): JsValue = {
        Json.toJson(o.format(dateF))
      }

    }


    implicit lazy val fRapidAddress: JSFormat[RapidAddress] = Json.format[RapidAddress]
    implicit lazy val fActivity: JSFormat[Activity] = Json.format[Activity]
    implicit lazy val fActivityEvent: JSFormat[ActivityEvent] = Json.format[ActivityEvent]
    implicit lazy val fActivityEventWithRobotCycle: JSFormat[ActivityEventWithRobotCycle] = Json.format[ActivityEventWithRobotCycle]
    implicit lazy val fIncomingCycleEvent: JSFormat[IncomingCycleEvent] = Json.format[IncomingCycleEvent]
    implicit lazy val fOutgoingCycleEvent: JSFormat[OutgoingCycleEvent] = Json.format[OutgoingCycleEvent]
    implicit lazy val fWorkCell: JSFormat[WorkCell] = Json.format[WorkCell]
    implicit lazy val fRobot: JSFormat[Robot] = Json.format[Robot]
    implicit lazy val fWorkCellCycle: JSFormat[WorkCellCycle] = Json.format[WorkCellCycle]
    implicit lazy val fWorkCellActivity : JSFormat[WorkCellActivity] = Json.format[WorkCellActivity]
    implicit lazy val fTimeSpan: JSFormat[TimeSpan] = Json.format[TimeSpan]
    implicit lazy val fRobotCyclesResponse: JSFormat[RobotCyclesResponse] = Json.format[RobotCyclesResponse]
    implicit lazy val fNewSignalState: JSFormat[NewSignalState] = Json.format[NewSignalState]
    implicit lazy val fSignalAddress: JSFormat[SignalAddress] = Json.format[SignalAddress]
    implicit lazy val fPointerChangedEvent: JSFormat[PointerChangedEvent] = Json.format[PointerChangedEvent]
    implicit lazy val fPointerPosition: JSFormat[PointerPosition] = Json.format[PointerPosition]
    implicit lazy val fPosition: JSFormat[Position] = Json.format[Position]
    implicit lazy val fRange: JSFormat[Range] = Json.format[Range]
    implicit lazy val fLocation: JSFormat[Location] = Json.format[Location]

    implicit  val fModule: JSFormat[Module] = Json.format[Module]
    implicit  val fTaskWithModules : JSFormat[TaskWithModules] = Json.format[TaskWithModules]
    implicit  val fModulesReadEvent: JSFormat[ModulesReadEvent] = Json.format[ModulesReadEvent]
    implicit lazy val fPointerWithInstruction: JSFormat[PointerWithInstruction] = Json.format[PointerWithInstruction]
    implicit lazy val fPointerWithIsWaiting: JSFormat[PointerWithIsWaiting] = Json.format[PointerWithIsWaiting]

    implicit lazy val fLoadWorkCells: JSFormat[LoadWorkCells] = Json.format[LoadWorkCells]
    /*implicit  val rRapidAddress : Reads[RapidAddress]= (
      (__ \ 'domain).read[String] and
        (__ \ 'kind).read[String] and
        (__ \ 'path).read[List[String]]
      )(RapidAddress)
    implicit  val rModule : Reads[Module]= (
      (__ \ 'name).read[String] and
        (__ \ 'file).read[List[String]]
    )(Module)
    implicit  val rTaskWithModules : Reads[TaskWithModules]= (
      (__ \ 'name).read[String] and
        (__ \ 'module).read[List[Module]]
    )(TaskWithModules)
    implicit  val rModulesReadEvent : Reads[ModulesReadEvent] = (
      (__ \ 'robotId).read[String] and
        (__ \ 'workCellid).read[String] and
        (__ \ 'address).read[RapidAddress] and
        (__ \ 'readValue).read[List[TaskWithModules]]
    )(ModulesReadEvent)
*/
    implicit lazy val fRobotDataAddress: JSFormat[RobotDataAddress] = Json.format[RobotDataAddress]
    implicit lazy val frequestModules: JSFormat[requestModules] = Json.format[requestModules]
    implicit lazy val fWorkCellList : JSFormat[WorkCellList] = Json.format[WorkCellList]
    implicit lazy val frequestWorkCellList: JSFormat[requestWorkCellList.type ] = deriveCaseObject[requestWorkCellList.type]
    implicit lazy val fLoadLog: JSFormat[LoadLog] = Json.format[LoadLog]
    implicit lazy val fLoadRobotModules: JSFormat[LoadRobotModules] = Json.format[LoadRobotModules]
    implicit lazy val fEmptyMessage: JSFormat[EmptyMessage.type] = deriveCaseObject[EmptyMessage.type]
    implicit lazy val fPlayLogs: JSFormat[PlayLogs.type] = deriveCaseObject[PlayLogs.type]
    implicit lazy val fFinished: JSFormat[Finished.type] = deriveCaseObject[Finished.type]
    implicit lazy val fStarted: JSFormat[Started.type] = deriveCaseObject[Started.type]
    implicit lazy val fConnect: JSFormat[Connect.type] = deriveCaseObject[Connect.type]
    implicit lazy val fStopPlayingLogs: JSFormat[StopPlayingLogs.type] = deriveCaseObject[StopPlayingLogs.type]

    def fRequest: JSFormat[Request] = Json.format[Request]
    def fMessage: JSFormat[Message] = Json.format[Message]
    def fResponse: JSFormat[Response] = Json.format[Response]

  }
  object WorkCell {
    implicit lazy val fWorkCell: JSFormat[WorkCell] = Formats.fWorkCell
  }
  object WorkCellList {
    implicit lazy val fWorkCell: JSFormat[WorkCellList] = Formats.fWorkCellList
  }
  object IncomingCycleEvent{
    implicit lazy val fIncomingCycleEvent: JSFormat[IncomingCycleEvent] = Formats.fIncomingCycleEvent

  }

  object PointerChangedEvent{
    implicit lazy val fPointerChangedEvent: JSFormat[PointerChangedEvent] = Formats.fPointerChangedEvent

  }
  object ModulesReadEvent{
    implicit lazy val fModulesReadEvent: JSFormat[ModulesReadEvent] = Formats.fModulesReadEvent

  }

  object Request {
    implicit lazy val fRequest: JSFormat[Request] = Formats.fRequest
  }

  object Response {
    implicit lazy val fRequest: JSFormat[Response] = Formats.fResponse
  }

  object Message {
    implicit lazy val fMessage: JSFormat[Message] = Formats.fMessage
  }
}

