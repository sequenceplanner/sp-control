package sp.virtcom

import akka.actor._
import sp.domain._
import sp.domain.Logic._

import scala.concurrent.Future
import akka.util._
import akka.pattern.ask
import sp.virtcom.APIBDDVerifier

import scala.concurrent._
import scala.concurrent.duration._
import scala.util._

class BDDVerifier extends Actor
  with ActorLogging with
  sp.service.ServiceSupport {

  val instanceID = ID.newID


  val statusResponse = BDDVerifierInfo.attributes.copy(
    instanceID = Some(this.instanceID)
  )
  triggerServiceRequestComm(statusResponse)

  subscribe(APIBDDVerifier.topicRequest)

  //--------------------------------------------------------------
  var bdds: Map[String, Map[String, Int] => Option[Boolean]] = Map()


  def receive = {
    case x: String =>
      val bodyAPI = for {
        mess <- SPMessage.fromJson(x)
        h <- mess.getHeaderAs[SPHeader] if h.to == instanceID.toString || h.to == APIBDDVerifier.service
        b <- mess.getBodyAs[APIBDDVerifier.Request]
      } yield {
        var spHeader = h.swapToAndFrom
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))

        b match {
          case APIBDDVerifier.RegisterBDD(name , bdd) =>
            Register(name ,bdd)
          case APIBDDVerifier.VerifyBDD(bddName , partialState ) =>
            Verify(bddName, partialState)
            //spHeader = SPHeader(from = "BDDVerifier", to = modelID, reply = SPValue("BDDVerifier"))
            //sendUpdatedModel(SPMessage.makeJson(spHeader, mapi.PutItems(generateSOPs(selectedSchedules,ids))))
        }
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))
      }
  }
  def sendAnswer(mess: String) = publish(APIVolvoScheduler.topicResponse, mess)
 // def sendUpdatedModel(mess: String) = publish(mapi.topicRequest, mess)


  // import context.dispatcher
  def Verify(bddName :String, partialState : Map[String, Int]) {
    val f = bdds.get(bddName).getOrElse((_ => None): Map[String, Int] => Option[Boolean])
    println("\n f  :    " + partialState)
    println("\n PState:    " + partialState)
    val resp = f(partialState) match {
      case Some(result) => (List(), SPAttributes("silent" -> true, "result" -> result))
      case None => (List(), SPAttributes("silent" -> true, "invalid" -> true))
    }
  }

  def Register(name : String, bdd : Map[String, Int] => Option[Boolean]){
    println("BDD " + name + " registered")
    bdds += name -> bdd
  }

}

object BDDVerifier {
  def props = Props(classOf[BDDVerifier])
}






