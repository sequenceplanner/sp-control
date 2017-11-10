package sp.robotservices

import akka.actor.{Actor, ActorLogging, Props}
import sp.domain.{ID, SPHeader, SPMessage}

class CycleChange extends Actor with ActorLogging with CycleChangeLogic with
  sp.service.ServiceSupport {
  val instanceID = ID.newID


  // Setting up the status response that is used for identifying the service in the cluster
  val statusResponse = CycleChangeInfo.attributes.copy(
    instanceID = Some(this.instanceID)
  )
  triggerServiceRequestComm(statusResponse)

  subscribe(APIRobotServices.topic)


  def receive = {
    // enable the line below for troubleshooting
    //case mess @ _ if {println(s"ExampleService MESSAGE: $mess from $sender"); false} => Unit

    case x: String =>
      // extract the body if it is a case class from my api as well as the header.to has my name
      // act on the messages from the API. Always add the logic in a trait to enable testing
      val bodyAPI = for {
        mess <- SPMessage.fromJson(x)
        h <- mess.getHeaderAs[SPHeader]
        b <- mess.getBodyAs[APIRobotServices.Message]
      } yield {
        handleMessage(b)

      }

  }

}
object CycleChange {
  def props = Props[CycleChange]
}

object CycleChangeInfo {
  import sp.domain.SchemaLogic._

  import sp.domain._


  val apischema =  SPAttributes()
  val attributes: APISP.StatusResponse = APISP.StatusResponse(
    service = "CycleChange",
    tags = List("CycleChange"),
    api = apischema,
    version = 1,
    topicRequest = APIRobotServices.topicRequest,
    topicResponse = APIRobotServices.topic,
    attributes = SPAttributes.empty
  )
}


trait CycleChangeLogic extends Actor with ActorLogging with sp.service.ServiceSupport{

  // Type aliases
  type Id = String
  type Instruction = String
  type RobotId = String
  type WorkCellId = String

  // Config values
  val homePosSignals = APIRobotServices.homePosSignals //Config.config.getStringList("services.cycleChange.homePosSignals")

  // State
  var getWorkCellsFlag: Boolean = true
  var cycleIdMap: Map[WorkCellId, Id] = Map.empty
  var workCellMap: Map[WorkCellId, List[RobotId]] = Map.empty
  var workCellStopFlagMap: Map[WorkCellId, Boolean] = Map.empty
  var robotStartFlagMap: Map[WorkCellId, Map[RobotId, Boolean]] = Map.empty

  def handleMessage(event: APIRobotServices.Message)=
  {
    event match {
      case event: APIRobotServices.IncomingCycleEvent =>  if (!workCellMap.contains(event.workCellId))
        requestWorkCells()
      else
        convert(event)
      case workCellList: APIRobotServices.WorkCellList =>
        workCellList.workcells.foreach{workCell =>
        workCellMap += (workCell.id -> workCell.robots.map(r => r.id))
        cycleIdMap = handleCycleIdMap(cycleIdMap, workCell.id)
        workCellStopFlagMap += (workCell.id -> true)
      }
        initializeRobotStartFlagMap()
        getWorkCellsFlag = !getWorkCellsFlag
          case _ => 0
    }


  }

  def handleCycleIdMap(map: Map[WorkCellId, Id], workCellId: WorkCellId): Map[WorkCellId, Id] = {
    var result = Map[WorkCellId, Id]()
    if (map.contains(workCellId))
      result = map
    else
      result = map + (workCellId -> uuid)
    result
  }

  def initializeRobotStartFlagMap() = {
    workCellMap.foreach{element =>
      var robotMap: Map[RobotId, Boolean] = Map.empty
      element._2.foreach{robotId =>
        robotMap += (robotId -> false)
      }
      robotStartFlagMap += (element._1 -> robotMap)
    }
  }

  def convert(event: APIRobotServices.IncomingCycleEvent) = {
    val isStartOrStop: Option[Boolean] = evaluateIsStart(event)
    if (isStartOrStop.isDefined) {
      val isStart = isStartOrStop.get
      if (isStart)
        cycleIdMap += (event.workCellId -> uuid)
      val cycleId = cycleIdMap(event.workCellId)
      val outgoingCycleEvent = APIRobotServices.OutgoingCycleEvent(cycleId, isStart, event.time, event.workCellId)
      //val json = write(outgoingCycleEvent)
      //log.info("From cycleChange: " + json)
      //sendToBus(json)
      publish(APIRobotServices.topic, SPMessage.makeJson(SPHeader(),outgoingCycleEvent))
    }
  }

  def evaluateIsStart(event: APIRobotServices.IncomingCycleEvent): Option[Boolean] = {
    var result: Option[Boolean] = None
    var robotMap = robotStartFlagMap(event.workCellId)
    robotMap += (event.robotId -> (event.newSignalState.value > 0))
    robotStartFlagMap += (event.workCellId -> robotMap)
    val flagList: List[Boolean] = robotStartFlagMap(event.workCellId).flatMap(element => List(element._2)).toList
    if (flagList.forall(_ == true)) {
      workCellStopFlagMap += (event.workCellId -> true)
      result = Some(false)
    }
    else if (flagList.count(_ == false) == 1 && workCellStopFlagMap(event.workCellId)) {
      workCellStopFlagMap += (event.workCellId -> false)
      result = Some(true)
    }
    result
  }

  def requestWorkCells() = {
    //import org.json4s.JsonDSL._
    getWorkCellsFlag = !getWorkCellsFlag
    val reqWorkCellList = APIRobotServices.requestWorkCellList
   // val json = ("event" -> "newWorkCellEncountered") ~ ("service" -> "cycleChange")
    log.info("CYCLE CHANGE REQUESTING WORK CELL LIST: ")
    //sendToBusWithTopic(settings.activeMQRequestTopic, write(json))
    publish(APIRobotServices.topicRequest,SPMessage.makeJson(SPHeader(),reqWorkCellList))
  }

  def uuid: String = java.util.UUID.randomUUID.toString

}
