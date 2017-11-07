package sp.robotservices

import akka.actor.{Actor, ActorLogging, Props}
import sp.domain.{ID, SPHeader, SPMessage}

class RoutineExtractor extends Actor with ActorLogging with RoutineExtractorLogic with
  sp.service.ServiceSupport {
  val instanceID = ID.newID


  // Setting up the status response that is used for identifying the service in the cluster
  val statusResponse = RoutineExtractorInfo.attributes.copy(
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
        h <- mess.getHeaderAs[SPHeader]  // only extract body if it is to me
        b <- mess.getBodyAs[APIRobotServices.Message]
      } yield {b match {
        case event: APIRobotServices.PointerChangedEvent =>
         // log.info(s"Routine extractor handling $event")

          activityIdMap = handleActivityIdMap(activityIdMap, event)
          handleEvent(event)
        case _ => 0
      }
      }

  }
}


object RoutineExtractor {
  def props = Props[RoutineExtractor]
}

object RoutineExtractorInfo {
  import sp.domain._


  val apischema =  SPAttributes()
  val attributes: APISP.StatusResponse = APISP.StatusResponse(
    service = "RoutineExtractor",
    tags = List("RoutineExtractor"),
    api = apischema,
    version = 1,
    topicRequest = APIRobotServices.topicRequest,
    topicResponse = APIRobotServices.topic,
    attributes = SPAttributes.empty
  )
}


trait RoutineExtractorLogic extends sp.service.ServiceSupport with Actor with ActorLogging{
  // Type aliases
  type RobotName = String
  type Id = String

  // Config file
  val waitRoutines = APIRobotServices.routinesToIgnore // Config.config.getStringList("services.routineChange.routinesToIgnore")

  // Variables
  var activityIdMap: Map[RobotName, Map[String, Id]] = Map.empty
  var priorEventMap: Map[RobotName, APIRobotServices.PointerChangedEvent] = Map.empty
  val isStart: Boolean = true



  def handleActivityIdMap(map: Map[RobotName, Map[String, Id]], event: APIRobotServices.PointerChangedEvent):
  Map[RobotName, Map[String, Id]] = {
    var result = Map[RobotName, Map[String, Id]]()
    if (map.contains(event.robotId))
      result = map
    else
      result = map + (event.robotId -> Map[String, Id]("current" -> uuid))
    result
  }

  def handleEvent(event: APIRobotServices.PointerChangedEvent) = {
    if (priorEventMap.contains(event.robotId)) {
      val robID: String = event.robotId
      val priorEvent = priorEventMap(event.robotId)
      val priorModule: String = priorEvent.programPointerPosition.position.module
      val currentModule: String = event.programPointerPosition.position.module
      val priorRoutine: String = robID + "_" + priorEvent.programPointerPosition.position.routine
      val currentRoutine: String = robID + "_" + event.programPointerPosition.position.routine
      if (!(priorRoutine == currentRoutine)) {
        activityIdMap = updateActivityIdMap(activityIdMap, event.robotId)
        val priorId = activityIdMap(event.robotId)("prior")
        val currentId = activityIdMap(event.robotId)("current")
        if (!isWaitingRoutine(priorRoutine)) {
          val routineStopEvent =
            APIRobotServices.ActivityEvent(priorId, !isStart,priorRoutine, event.robotId, event.programPointerPosition.time,
              "routines", event.workCellId)
          //val json = write(routineStopEvent)
          // log.info("Previous routine: " + json)
          //sendToBus(json)
          log.info(s"Routine extracto ${routineStopEvent}")
          publish(APIRobotServices.topic,SPMessage.makeJson(SPHeader(),routineStopEvent))
        }
        if (!isWaitingRoutine(currentRoutine)) {
          val routineStartEvent =
            APIRobotServices.ActivityEvent(currentId, isStart, currentRoutine, event.robotId, event.programPointerPosition.time,
              "routines", event.workCellId)
          //val json = write(routineStartEvent)
          // log.info("Current routine: " + json)
          //sendToBus(json)
          log.info(s"Routine extracto ${routineStartEvent}")

          publish(APIRobotServices.topic,SPMessage.makeJson(SPHeader(),routineStartEvent))

        }
      }
    }
    priorEventMap += (event.robotId -> event)
  }

  def updateActivityIdMap(map: Map[RobotName, Map[String, Id]], robotId: String): Map[RobotName, Map[String, Id]] = {
    var result = map
    val temp = result(robotId)("current")
    result += (robotId -> Map[String,Id]("current" -> uuid, "prior" -> temp))
    result
  }

  def isWaitingRoutine(routineName: String): Boolean = {
    var flag = false
    if (waitRoutines.contains(routineName))
      flag = true
    flag
  }

  def uuid: String = ID.newID.toString
}


