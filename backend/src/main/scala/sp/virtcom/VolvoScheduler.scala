package sp.virtcom

import akka.actor._
import play.api.libs.json._
import sp.domain._
import sp.domain.logic._
import sp.virtcom.SOPGenerator._
import sp.models.{APIModel => mapi}
import sp.virtcom.APIVolvoScheduler.gotCases
import sp.virtcom.Calculate._

class VolvoScheduler extends Actor
  with ActorLogging with
  sp.service.ServiceSupport {

  val instanceID = ID.newID


  val statusResponse = VolvoSchedulerInfo.attributes.copy(
    instanceID = Some(this.instanceID)
  )
  triggerServiceRequestComm(statusResponse)

  subscribe(APIVolvoScheduler.topicRequest)

  def receive = {

    case x: String =>
      val bodyAPI = for {
        mess <- SPMessage.fromJson(x)
        h <- mess.getHeaderAs[SPHeader] if h.to == instanceID.toString || h.to == APIVolvoScheduler.service
        b <- mess.getBodyAs[APIVolvoScheduler.Request]
      } yield {
        var spHeader = h.swapToAndFrom
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))

        b match {
          case APIVolvoScheduler.generateSOPs(modelID, selectedSchedules, ids) =>
            spHeader = SPHeader(from = "VolvoScheduler", to = modelID, reply = SPValue("VolvoScheduler"))
            sendUpdatedModel(SPMessage.makeJson(spHeader, mapi.PutItems(generateSOPs(selectedSchedules,ids))))
          case APIVolvoScheduler.getCases(sopID, ids) =>
            spHeader = SPHeader(from = "VolvoScheduler", to = "VolvoSchedulerWidget", reply = SPValue("VolvoScheduler"))
            sendAnswer(SPMessage.makeJson(spHeader, gotCases(GetCases.cases(sopID, ids))))
          case APIVolvoScheduler.calculate(modelID, sopID, ids, neglectedCases) =>
            import scala.concurrent.ExecutionContext.Implicits.global
            for{res <- synthOpt(sopID : ID, ids : List[IDAble], neglectedCases : Set[ID])}
            yield {
              spHeader = SPHeader(from = "VolvoScheduler", to = modelID, reply = SPValue("VolvoScheduler"))
              sendUpdatedModel(SPMessage.makeJson(spHeader, mapi.PutItems(res._1)))
            }
        }
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))
      }
  }
  def sendAnswer(mess: String) = publish(APIVolvoScheduler.topicResponse, mess)
  def sendUpdatedModel(mess: String) = publish(mapi.topicRequest, mess)
}

object VolvoScheduler {
  def props = Props(classOf[VolvoScheduler])
}




