package sp.virtcom

import akka.actor._
import sp.domain._
import sp.virtcom.SOPGenerator._
import sp.models.{APIModel => mapi}
import sp.virtcom.APIVolvoScheduler.{gotCases, generatedSopID, calculateStructID, cpResults}
import sp.virtcom.Calculate._

class VolvoScheduler extends Actor  with ActorLogging with  sp.service.ServiceSupport {
  // initialization
  val instanceID = ID.newID
  val statusResponse = VolvoSchedulerInfo.attributes.copy( instanceID = Some(this.instanceID) )
  triggerServiceRequestComm(statusResponse)
  subscribe(APIVolvoScheduler.topicRequest)

  def receive = {

    case x: String =>
      for { // unpack message
        mess <- SPMessage.fromJson(x)
        h <- mess.getHeaderAs[SPHeader] if h.to == instanceID.toString || h.to == APIVolvoScheduler.service
        b <- mess.getBodyAs[APIVolvoScheduler.Request]
      } yield {
        var spHeader = h.swapToAndFrom
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK())) // acknowledge message received

        b match { // Check if the body is any of the following classes, and execute program
          case APIVolvoScheduler.generateSOPs(modelID, selectedSchedules, ids) => // Create SOPs and Structs from IDAbles
            spHeader = SPHeader(from = "VolvoScheduler", to = modelID, reply = SPValue("VolvoScheduler"))
            val res = generateSOPs(selectedSchedules,ids) // generate SOPs and new struct
            sendUpdatedModel(SPMessage.makeJson(spHeader, mapi.PutItems(res._1))) // Update model with the new SOPs and Structs
            spHeader = SPHeader(from = "VolvoScheduler", to = "VolvoSchedulerWidget", reply = SPValue("VolvoScheduler"))
            sendAnswer(SPMessage.makeJson(spHeader, generatedSopID(res._2))) // Send SOP ID to GUI

          case APIVolvoScheduler.getCases(sopID, ids) =>  // Find all Cases/branching alternatives of the main SOP
            spHeader = SPHeader(from = "VolvoScheduler", to = "VolvoSchedulerWidget", reply = SPValue("VolvoScheduler"))
            sendAnswer(SPMessage.makeJson(spHeader, gotCases(GetCases.cases(sopID, ids)))) // Send to GUI

          case APIVolvoScheduler.calculate(modelID, sopID, ids, neglectedCases) => // Synthesize and optimize the SOPs
            import scala.concurrent.ExecutionContext.Implicits.global
            for{res <- synthOpt(sopID : ID, ids : List[IDAble], neglectedCases : Set[ID])}
            yield {
              spHeader = SPHeader(from = "VolvoScheduler", to = modelID, reply = SPValue("VolvoScheduler"))
              sendUpdatedModel(SPMessage.makeJson(spHeader, mapi.PutItems(res._1))) // Update Model with new IDAbles
              spHeader = SPHeader(from = "VolvoScheduler", to = "VolvoSchedulerWidget", reply = SPValue("VolvoScheduler"))
              sendAnswer(SPMessage.makeJson(spHeader, calculateStructID(res._3))) // Send the ID of the struct - to GUI - where the new IDAbles are contained
              sendAnswer(SPMessage.makeJson(spHeader, cpResults(res._2))) //  Send the optimization results to GUI
            }
        }
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK())) // should perhaps make more acknowledgements... one for each sent message?
      }
  }
  def sendAnswer(mess: String) = publish(APIVolvoScheduler.topicResponse, mess)
  def sendUpdatedModel(mess: String) = publish(mapi.topicRequest, mess)
}

object VolvoScheduler {
  def props = Props(classOf[VolvoScheduler])
}




