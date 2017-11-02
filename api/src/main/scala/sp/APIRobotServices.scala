package sp.robotservices

import java.time._

import sp.domain.JSFormat
import java.text.SimpleDateFormat

import sp.domain.Logic.deriveCaseObject

object APIRobotServices{

  sealed trait Request
  sealed trait Message
  val vdService = "VDservice"
  val instructionFillerService = "Instruction Filler"
  val logPlayer = "Log Player"
  val topic = "Lisa"
  val topicRequest = "LisaRequest"


  val activeMQUrl = "localhost"
  val activeMQPort = 61616
  val activeMQTopic = "LISA"
  val activeMQRequestTopic = " request"
  val routinesToIgnore: List[String] = List("testRout1", "testRout2")
  val homePosSignals: List[String] = List("O_Homepos", "R2UT_HomeAboveBP", "R4UT_HomePosLeft", "R5UT_HomePos")

//frontend
  case class LoadLog(path:String = "/home/ashfaqf/Projects/Lisa files/from_volvo/logs/log-13_12_35") extends Request
  case class LoadRobotModules(folderPath: String = "/home/ashfaqf/Projects/Lisa files/from_volvo/logs/robotProgs/w1741060") extends Request
  case object PlayLogs extends Request


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
                          path: List[String])

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
  case class ModulesReadEvent(robotId: String,
                              workCellId: String,
                              address: RapidAddress,
                              readValue: List[TaskWithModules]) extends Message

  case class TaskWithModules(name: String,
                             modules: List[Module])

  case class Module(name: String,
                    file: List[String])


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
    //implicit val yourJodaDateReads = Reads.jodaDateReads("yyyy-MM-dd'T'HH:mm:ss'Z'")
    //implicit val yourJodaDateWrites = Writes.jodaDateWrites("yyyy-MM-dd'T'HH:mm:ss'Z")

    lazy val dateF = format.DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
    implicit lazy val javatimeF = new Format[ZonedDateTime] {
      override def reads(json: JsValue): JsResult[ZonedDateTime] = {
        json.validate[String].map(ZonedDateTime.parse(_, dateF))
      }

      override def writes(o: ZonedDateTime): JsValue = {
        Json.toJson(o.format(dateF))
      }

    }



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
    implicit lazy val fModulesReadEvent: JSFormat[ModulesReadEvent] = Json.format[ModulesReadEvent]
    implicit lazy val fTaskWithModules : JSFormat[TaskWithModules] = Json.format[TaskWithModules]
    implicit lazy val fModule: JSFormat[Module] = Json.format[Module]
    implicit lazy val fPointerWithInstruction: JSFormat[PointerWithInstruction] = Json.format[PointerWithInstruction]
    implicit lazy val fPointerWithIsWaiting: JSFormat[PointerWithIsWaiting] = Json.format[PointerWithIsWaiting]

    implicit lazy val fRapidAddress: JSFormat[RapidAddress] = Json.format[RapidAddress]
    implicit lazy val fRobotDataAddress: JSFormat[RobotDataAddress] = Json.format[RobotDataAddress]
    implicit lazy val frequestModules: JSFormat[requestModules] = Json.format[requestModules]

    implicit lazy val frequestWorkCellList: JSFormat[requestWorkCellList.type ] = deriveCaseObject[requestWorkCellList.type]
    implicit lazy val fLoadLog: JSFormat[LoadLog] = Json.format[LoadLog]
    implicit lazy val fLoadRobotModules: JSFormat[LoadRobotModules] = Json.format[LoadRobotModules]
    implicit lazy val fEmptyMessage: JSFormat[EmptyMessage.type] = deriveCaseObject[EmptyMessage.type]
    implicit lazy val fPlayLogs: JSFormat[PlayLogs.type] = deriveCaseObject[PlayLogs.type]

    def fRequest: JSFormat[Request] = Json.format[Request]
    def fMessage: JSFormat[Message] = Json.format[Message]
  }

  object Request {
    implicit lazy val fRequest: JSFormat[Request] = Formats.fRequest
  }

  object Message {
    implicit lazy val fMessage: JSFormat[Message] = Formats.fMessage
  }
}
