package sp.robotservices

import java.io.File

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.codemettle.reactivemq.ReActiveMQExtension
import com.codemettle.reactivemq.ReActiveMQMessages._
import com.codemettle.reactivemq.model.{AMQMessage, Topic}
import play.api.libs.json.{JsArray, JsNull, JsUndefined, JsValue, Json}
import sp.domain.{ID, SPHeader, SPMessage}

import scala.io.Source

class LogPlayer extends  Actor with ActorLogging with
  sp.service.ServiceSupport{
  val instanceID = ID.newID

  // Setting up the status response that is used for identifying the service in the cluster
  val statusResponse = LogPlayerInfo.attributes.copy(
    instanceID = Some(this.instanceID)
  )
  triggerServiceRequestComm(statusResponse)

  subscribe(APIRobotServices.topicRequest)
  // State
  var theBus: Option[ActorRef] = None
  ReActiveMQExtension(context.system).manager ! GetConnection(s"nio://${APIRobotServices.activeMQUrl}:${APIRobotServices.activeMQPort}")

  var jsonFile: JsValue = JsNull
  var wcellfile: List[JsValue] = List.empty
  var rcdProgs: List[JsValue] = List.empty
  var evts: List[JsValue] = List.empty
  var fileLoaded= false
  var wcellmap: Map[String,APIRobotServices.WorkCell] =Map.empty
  var robotIdToModues: Map[String,APIRobotServices.ModulesReadEvent] = Map.empty
  var playLog :Boolean = true
  def receive = {
    case ConnectionEstablished(request, c) =>
      log.info("Connected: " + request)
      c ! ConsumeFromTopic(APIRobotServices.activeMQTopic)
      theBus = Some(c)
    case ConnectionFailed(request, reason) =>
      log.error("Connection failed: " + reason)
    //case mess@AMQMessage(body, prop, headers) =>


    case x: String =>
      // extract the body if it is a case class from my api as well as the header.to has my name
      // act on the messages from the API. Always add the logic in a trait to enable testing
      val bodyAPI = for {
        mess <- SPMessage.fromJson(x)
        h <- mess.getHeaderAs[SPHeader] if (h.to == APIRobotServices.logPlayer)
        b <- mess.getBodyAs[APIRobotServices.Request]
      } yield {
        b match {
          case l: APIRobotServices.LoadLog =>
            jsonFile = loadFile(l.path)

            evts =
              if (jsonFile != JsNull)
                jsonFile.as[JsArray].value.filterNot(has(_,"readValue")).filterNot(has(_,"workCells")).toList
              else
                List.empty

          case l: APIRobotServices.LoadRobotModules => loadRobotModules(l.folderPath)
          case  APIRobotServices.PlayLogs =>
            def playLog ={
              def playFirstEvent(e: List[JsValue]): Unit ={
                e match {
                  case x::xs => sendToBusWithTopic (APIRobotServices.activeMQTopic, x.toString () )
                playFirstEvent (xs)
                  case Nil =>
                }
                playFirstEvent(evts)
              }
            }
          case _ =>
        }

      }
  }

  def has(json:JsValue,childString: String): Boolean = {
    if ((json \ childString).isInstanceOf[JsUndefined])
      true
    else
      false
  }

  def loadRobotModules(path: String): Unit ={
    def getListOfFiles(dir: String):List[File] = {
      val d = new File(dir)
      if (d.exists && d.isDirectory) {
        d.listFiles.filter(_.isFile).toList
      } else {
        List[File]()
      }
    }
    getListOfFiles(path).foreach(f => publish(APIRobotServices.topic,SPMessage.makeJson(SPHeader(),Source.fromFile(f).getLines.mkString)))
  }
  def sendToBusWithTopic(topic: String, json: String) = {
    theBus.foreach{bus => bus ! SendMessage(Topic(topic), AMQMessage(json))}
  }

  def robotModules(json:List[JsValue]){
    val modules = json.map(_.asInstanceOf[APIRobotServices.ModulesReadEvent])
    modules.foreach(x => robotIdToModues += (x.robotId -> x))
  }
  def loadFile(path: String): JsValue ={
    import scala.io.Source
    println(s"loading $path")
    import java.nio.file.{Paths, Files}

    if (!Files.exists(Paths.get(path))) {
      JsNull
    }
    else {
      val source: String = Source.fromFile(path).getLines.mkString
      val parsedFile = Json.parse(source)
      println("Finished parsing file")
      parsedFile
    }
  }





}
object LogPlayer {
  def props = Props[LogPlayer]
}

object LogPlayerInfo {
  import sp.domain.SchemaLogic._

  import sp.domain._


  val apischema =  SPAttributes()
  val attributes: APISP.StatusResponse = APISP.StatusResponse(
    service = "LogPlayer",
    tags = List("LogPlayer"),
    api = apischema,
    version = 1,
    topicRequest = APIRobotServices.topicRequest,
    topicResponse = APIRobotServices.topic,
    attributes = SPAttributes.empty
  )
}