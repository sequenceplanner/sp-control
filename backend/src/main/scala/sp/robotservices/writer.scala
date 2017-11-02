package sp.robotservices

import akka.actor.{Actor, ActorLogging, Props}
import com.github.tototoshi.csv.CSVWriter
import sp.domain.{ID, SPHeader, SPMessage}

class Writer extends Actor with ActorLogging with sp.service.ServiceSupport {
  val instanceID = ID.newID


  // Setting up the status response that is used for identifying the service in the cluster
  val statusResponse = WriterInfo.attributes.copy(
    instanceID = Some(this.instanceID)
  )
  triggerServiceRequestComm(statusResponse)

  subscribe(APIRobotServices.topicRequest)
  val csvFile = CSVWriter.open("../testFiles/events.csv")


  type RobotId = String
  type WorkCellId = String
  type ActivityType = String
  type ActivityEvents = List[APIRobotServices.ActivityEvent]
  type Activities = List[APIRobotServices.Activity]



  var robotActivityMap : Map[RobotId,String] = Map.empty
  var robotIdToCurrentPos : Map[RobotId,Boolean] = Map.empty
  var robotIdToCurrentRobotCycle : Map[RobotId,String] = Map.empty


  def allRobotsAtHome():Boolean ={
    robotIdToCurrentPos.values.foreach(print(_))
    robotIdToCurrentPos.values.forall(_ == true)
  }
  def updateActivityMap(evt:APIRobotServices.ActivityEvent):Unit ={
    robotActivityMap += (evt.robotId -> evt.name)

  }
  def allRobotsRunMain():Boolean = {
    robotActivityMap.values.foreach(print(_))
    robotActivityMap.values.forall(_.contains("main"))

  }
  val homePosSignals = APIRobotServices.homePosSignals
  var cycleId = uuid

  override def postStop()={
    csvFile.close()
    super.postStop()
  }

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
      } yield {
        b match {
          case event: APIRobotServices.ActivityEvent =>
            updateActivityMap(event)
            if (event.isStart && allRobotsRunMain() && allRobotsAtHome() /*robotIdToCurrentPos.getOrElse(event.robotId,false)*/ ) {
              //robotIdToCurrentRobotCycle += (event.robotId -> uuid) // (robotIdToCurrentRobotCycle.getOrElse(event.robotId,-1) + 1) )
              cycleId = uuid
            }

            def robCylId = robotIdToCurrentRobotCycle.contains(event.robotId) match {
              case true => robotIdToCurrentRobotCycle(event.robotId)
              case false => robotIdToCurrentRobotCycle += (event.robotId -> uuid)
                robotIdToCurrentRobotCycle(event.robotId)

            }

            writeToCSV(List(event.activityId, cycleId, event.isStart, event.name, event.robotId, event.time, event.`type`, event.workCellId, robotIdToCurrentPos(event.robotId)))

        }
      }

  }
  def uuid: String = ID.newID.toString

  def writeToCSV(row:List[Any]):Unit={
    csvFile.writeRow(row)
  }




}
object Writer {
  def props = Props[Writer]
}

object WriterInfo {
  import sp.domain._


  val apischema =  SPAttributes()
  val attributes: APISP.StatusResponse = APISP.StatusResponse(
    service = "Writer",
    tags = List("Writer"),
    api = apischema,
    version = 1,
    topicRequest = APIRobotServices.topicRequest,
    topicResponse = APIRobotServices.topic,
    attributes = SPAttributes.empty
  )
}