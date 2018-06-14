package sp.modelImport

import akka.actor._
import sp.modelImport.oldDomain._
import OldModel._
import OldModel.Formats._
import play.api.libs.json._
import sp.models.{APIModel => mapi}

class SPModelImport extends Actor
  with ActorLogging with
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
        spHeader = SPHeader(from = "SPModelImport", to = "SPModelImportWidget", reply = SPValue("SPModelImport"))
        importModel(b, spHeader)
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))
      }
  }
  def sendAnswer(mess: String) = publish(APISPModelImport.topicResponse, mess)



  def importModel(body: APISPModelImport.Request, spHeader : SPHeader) = {
    body match {
      case APISPModelImport.ImportText(text) =>
        // If the model is of the old variety, change the Hierarchies to Structs
        val oldModel = Json.parse(text).asOpt[Model]
        if(oldModel.nonEmpty) {
          val model = oldModel.get
          val HierarchyRoots = model.ids.filter(_.isInstanceOf[HierarchyRoot]).map(_.asInstanceOf[HierarchyRoot]) // Get Hierarchy Roots from the model
          val Structs = HierarchyRoots.map(HR => Struct(HR.name, HR.children.map(C => getHierarchyNodeAsStructNodes(C, Some(HR.id))).flatten, HR.attributes, HR.id)) // convert them to Structs
          val newIDAbles = (model.ids.filterNot(_.isInstanceOf[HierarchyRoot]) ++ Structs) // remove Hierarchy roots from model IDAbles and add Structs instead

          sendAnswer(SPMessage.makeJson(spHeader, model.copy(ids = newIDAbles))) // Make a copy of the model with the new IDAbles and send
        }
        // otherwise import the new (previously exported) model
        else{
          import mapi.Formats.fModelToExport
          val nModel = Json.parse(text).asOpt[mapi.ModelToExport]
          if(nModel.nonEmpty)
            sendAnswer(SPMessage.makeJson(spHeader, nModel.get))
        }
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

object SPModelImport {
  def props = Props(classOf[SPModelImport])
}


