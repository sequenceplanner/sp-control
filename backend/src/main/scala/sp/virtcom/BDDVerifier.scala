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

object BDDVerifier {
  def props = Props(classOf[BDDVerifier])

  var bdds: Map[String, Map[String, Int] => Option[Boolean]] = Map()

  class BDDVerifier extends Actor
    with ActorLogging with
    sp.service.ServiceSupport with
    RegBDD {

    val instanceID = ID.newID


    val statusResponse = BDDVerifierInfo.attributes.copy(
      instanceID = Some(this.instanceID)
    )
    triggerServiceRequestComm(statusResponse)

    subscribe(APIBDDVerifier.topicRequest)

    //--------------------------------------------------------------



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
            case APIBDDVerifier.VerifyBDD(bddName, partialState) =>
              spHeader = SPHeader(from = "BDDVerifier", to = "VolvoSchedulerWidget", reply = SPValue("BDDVerifier"))
              sendAnswer(SPMessage.makeJson(spHeader, APIBDDVerifier.VerificationResult(Verify(bddName, partialState))))
          }
          sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))
        }
    }

    def sendAnswer(mess: String) = publish(APIBDDVerifier.topicResponse, mess)


    def Verify(bddName: String, partialState: Map[String, Int]) = { // Test if the given state is allowed
      val f = bdds.get(bddName).getOrElse((_ => None): Map[String, Int] => Option[Boolean])
      var res = false
      val resp = f(partialState) match {
        case Some(result) => res = result
        case None =>  res = false
      }
      res
    }

  }
}

trait RegBDD { // add bdd to BDDVerifier's bdds, for later verification
  def RegisterBDD(name : String, bdd : Map[String, Int] => Option[Boolean]){
    println("BDD " + name + " registered")
    BDDVerifier.bdds += name -> bdd
  }
}






