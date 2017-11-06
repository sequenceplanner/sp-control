package sp.robotservices


import sp.domain._
import akka.actor.{Actor, ActorLogging, Props}
import com.github.nscala_time.time.Imports._
import sp.domain.{SPHeader, SPMessage}



class InstructionFiller extends Actor with ActorLogging with InstructionFillerLogic with
sp.service.ServiceSupport {

  val instanceID = ID.newID

println("Instruction Filler ")
  // Setting up the status response that is used for identifying the service in the cluster
  val statusResponse = InstructionFillerInfo.attributes.copy(
    instanceID = Some(this.instanceID)
  )
  triggerServiceRequestComm(statusResponse)

  subscribe(APIRobotServices.topic)

  def receive = {
    // enable the line below for troubleshooting
    //case mess @ _ if {println(s"instructionfiller MESSAGE: $mess from $sender"); false} => Unit

    case x: String =>
      //log.info(s"Instrution filler got ${x}")
      // extract the body if it is a case class from my api as well as the header.to has my name
      // act on the messages from the API. Always add the logic in a trait to enable testing
      for {
        mess <- SPMessage.fromJson(x)
        h <- mess.getHeaderAs[SPHeader] if (h.from == APIRobotServices.vdService || h.from == APIRobotServices.logPlayer)  // only extract body if it is to me
        b <- mess.getBodyAs[APIRobotServices.Message]
      } yield {
        log.info(s"instruction filler $h")
        b match {
          case event: APIRobotServices.ModulesReadEvent => event.readValue.foreach(task => {
            task.modules.foreach(module => moduleMap += (module.name -> module))
            taskMap += (task.name -> moduleMap)
            moduleMap = Map.empty[ModuleName, APIRobotServices.Module]
            log.info(s"${moduleMap.keys.toList.mkString}")
          })
          case event:
            APIRobotServices.PointerChangedEvent =>
            log.info(s"instruction filling event $event")
            fill(event)
          case _ => println("Instruction filler: nothing to do")
            0

        }
      }

  }

  def sendAnswer(mess: String) = publish(APIRobotServices.topic, mess)


}


object InstructionFiller {
  def props = Props[InstructionFiller]
}

object InstructionFillerInfo {
  import sp.domain.SchemaLogic._

  import sp.domain._

  case class InstructionFillerRequest(request: APIRobotServices.Request)
  case class InstructionFillerMessage(response: APIRobotServices.Message)

  lazy val req: com.sksamuel.avro4s.SchemaFor[InstructionFillerRequest] = com.sksamuel.avro4s.SchemaFor[InstructionFillerRequest]
  lazy val resp: com.sksamuel.avro4s.SchemaFor[InstructionFillerMessage] = com.sksamuel.avro4s.SchemaFor[InstructionFillerMessage]

  val apischema = makeMeASchema(
    req(),
    resp()
  )
  val attributes: APISP.StatusResponse = APISP.StatusResponse(
    service = "Instruction Filler",
    tags = List("instruction filler"),
    api = apischema,
    version = 1,
    topicRequest = APIRobotServices.topicRequest,
    topicResponse = APIRobotServices.topic,
    attributes = SPAttributes.empty
  )
}

trait InstructionFillerLogic extends Actor with ActorLogging with sp.service.ServiceSupport{

  // Type aliases
  type RobotId = String
  type TaskName = String
  type ModuleName = String
  type Instruction = String
  type ActivityId = ID
  type WaitInstruction = String

  // Maps
  var robotMap: Map[RobotId, Map[TaskName, Map[ModuleName, APIRobotServices.Module]]] = Map.empty
  var taskMap: Map[TaskName, Map[ModuleName, APIRobotServices.Module]] = Map.empty
  var moduleMap: Map[ModuleName, APIRobotServices.Module] = Map.empty
  var timerMap: Map[RobotId, DateTime] = Map.empty

  // State
  var isWaiting: Map[RobotId, Option[(ActivityId, WaitInstruction)]] = Map.empty


  def fill(event: APIRobotServices.PointerChangedEvent) = {
    val eventPPPos = event.programPointerPosition
    if (robotMap.contains(event.robotId)) {
      if (robotMap(event.robotId).contains(eventPPPos.task)) {
        if (robotMap(event.robotId)(eventPPPos.task).contains(eventPPPos.position.module)) {
          val module: APIRobotServices.Module = robotMap(event.robotId)(eventPPPos.task)(eventPPPos.position.module)
          val range: APIRobotServices.Range = eventPPPos.position.range

          val instruction: Option[Instruction] = range.begin.row == range.end.row match{
            case true => Option(module.file.get(range.begin.row - 1).
              slice(range.begin.column - 1, range.end.column + 1))
            case false => module.file.get.slice(range.begin.row - 1, range.end.row - 1).headOption
          }
          val filledEvent: APIRobotServices.PointerWithInstruction =
            APIRobotServices.PointerWithInstruction(event.robotId, event.workCellId, event.address, instruction.getOrElse("None"), eventPPPos)
          //val json = write(filledEvent)
          fillWithIsWaiting(filledEvent)
        } else
          log.info(s"The system ${event.robotId} does not contain the module called" +
            s"${eventPPPos.position.module}")
      } else
        log.info(s"The system ${event.robotId} does not contain the task called" +
          s"${eventPPPos.task}")
    } else {
      if (timerMap.contains(event.robotId)) {
        if ((timerMap(event.robotId) to DateTime.now).millis < 60000) {
          timerMap += (event.robotId -> DateTime.now)
          requestModules(event.robotId)
        }
      } else {
        timerMap += (event.robotId -> DateTime.now)
        requestModules(event.robotId)
      }
    }
  }

  def requestModules(robotId: RobotId) = APIRobotServices.requestModules(robotId)

  //is waiting
  def fillWithIsWaiting(event: APIRobotServices.PointerWithInstruction) = {
    //fill for isWaiting
    val instruction: Instruction = event.instruction
    var isWaiting: Boolean = false
    if (instruction.startsWith("Wait") || instruction.startsWith("ExecEngine"))
      isWaiting = true
    val filledEvent = APIRobotServices.PointerWithIsWaiting(event.robotId, event.workCellId, event.address, instruction, isWaiting,
      event.programPointerPosition)
    //val json: String = write(APIRobotServices.topic,filledEvent)
    //println("From isWaiting: " + json)
    //sendToBus(json)

    checkIfWaitChange(filledEvent)
  }


  //wait change
  def checkIfWaitChange(event: APIRobotServices.PointerWithIsWaiting) = {
    //val event: PointerWithIsWaiting = json.extract[PointerWithIsWaiting]

    if (!isWaiting.contains(event.robotId)) {
      isWaiting += (event.robotId -> None)
    }

    if (isWaiting(event.robotId).isDefined != event.isWaiting) {
      val (activityId, waitInstruction): (ActivityId, WaitInstruction) = if (event.isWaiting) {
        val id = ID.newID
        isWaiting += (event.robotId -> Some((id, event.instruction)))
        (id, event.instruction)
      } else {
        val (id, instruction) = isWaiting(event.robotId).get
        isWaiting += (event.robotId -> None)
        (id, instruction)
      }
      val activityEvent = APIRobotServices.ActivityEvent(activityId.toString, event.isWaiting, waitInstruction, event.robotId,
        event.programPointerPosition.time, "wait", event.workCellId)
      println("From waitChange: " + activityEvent)
      val header = SPHeader(from = APIRobotServices.instructionFillerService )
      publish(APIRobotServices.topic,SPMessage.makeJson(header,activityEvent))
    }

  }

}
