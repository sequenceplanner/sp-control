package sp.modelImport

import akka.actor._
import sp.modelImport.oldDomain._
import OldModel._
import OldModel.Formats._


import play.api.libs.json._

class SPModelImport extends Actor
  with ActorLogging with
  SPModelImportLogic with
  sp.service.ServiceSupport {

  val instanceID = ID.newID


  val statusResponse = SPModelImportInfo.attributes.copy(
    instanceID = Some(this.instanceID)
  )
  triggerServiceRequestComm(statusResponse)

  subscribe(APISPModelImport.topicRequest)

  def receive = {

    case x: String =>
      val bodyAPI = for {
        mess <- SPMessage.fromJson(x)
        h <- mess.getHeaderAs[SPHeader] if h.to == instanceID.toString || h.to == APISPModelImport.service
        b <- mess.getBodyAs[APISPModelImport.Request] // Get message body as Import (text string)
      } yield {
        var spHeader = h.swapToAndFrom()
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))

        val toSend = commands(b)

        spHeader = SPHeader(from = "SPModelImport", to = "SPModelImportWidget", reply = SPValue("SPModelImport"))
        sendAnswer(SPMessage.makeJson(spHeader, toSend))
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))
      }
  }
  def sendAnswer(mess: String) = publish(APISPModelImport.topicResponse, mess)
}

object SPModelImport {
  def props = Props(classOf[SPModelImport])
}

trait SPModelImportLogic {

  def commands(body: APISPModelImport.Request) = {
    body match {
      case APISPModelImport.ImportText(text) =>
        val model = Json.parse(text).as[Model] // Parse the textstring as the type OldModel, using Json formatters
        val HierarchyRoots = model.ids.filter(_.isInstanceOf[HierarchyRoot]).map(_.asInstanceOf[HierarchyRoot]) // Get Hierarchy Roots from the model
        val Structs = HierarchyRoots.map(HR => Struct(HR.name, HR.children.map(C => getHierarchyNodeAsStructNodes(C, Some(HR.id))).flatten, HR.attributes, HR.id)) // convert them to Structs
        val newIDAbles = (model.ids.filterNot(_.isInstanceOf[HierarchyRoot]) ++ Structs) // remove Hierarchy roots from model IDAbles and add Structs instead
        model.copy(ids = newIDAbles) // Make a copy of the model with the new IDAbles
    }
  }

  def getHierarchyNodeAsStructNodes(hierarchyNode: HierarchyNode, parent: Option[ID]): List[StructNode] = {
    val structNode = StructNode(hierarchyNode.item, parent, hierarchyNode.id, SPAttributes()) // Create a Struct Node with the parent ID of the previous HierarchyNode
    var structNodes = List(structNode)
    hierarchyNode.children.foreach(c => structNodes ++= getHierarchyNodeAsStructNodes(c, Some(structNode.nodeID))) // Go through the current Hierarchy Node and add all its children as StructNodes
    structNodes
  }
}



object OldModel {
  import sp.modelImport.oldDomain.logic.JsonImplicit // Information about classes and JSon formaters, (the old domain includes Hierarchies, which was previously used in SP instead of structs)
  import sp.modelImport.oldDomain.Logic._

  case class ModelInfo(id: ID, name: String, version: Long, attributes: SPAttributes, history: List[SPAttributes])
  case class Model(model: ID, info: ModelInfo, ids: List[IDAble])

  object Formats { // Json formatters for the model
    implicit val fModelInfo: JSFormat[ModelInfo] = Json.format[ModelInfo]
    implicit val fModel: JSFormat[Model] = Json.format[Model]
  }
}