package sp.unification

import akka.actor._
import sp.domain.Logic._
import sp.domain._
import sp.runners._
import sp.unification.{APIUnification => api}

// This class is used to describe the desired behaviour for all model services

case class UnificationModelsComSupport(mName : String, mTags : List[String] = List(), mResources : List[ResourceAndAbilities], mSetupRunner : APIOperationRunner.CreateRunner) extends Actor  with ActorLogging with  sp.service.ServiceSupport {
  // initialization
  val instanceID = ID.newID
  val statusResponse = UnificationInfo.attributes.copy( instanceID = Some(this.instanceID) )
  triggerServiceRequestComm(statusResponse)
  subscribe(api.topicRequest)

  def receive = {

    case x: String =>
      for { // unpack message
        mess <- SPMessage.fromJson(x)
        h <- mess.getHeaderAs[SPHeader] if h.to == instanceID.toString || h.to == api.service
        b <- mess.getBodyAs[api.Request]
      } yield {
        var spHeader = h.swapToAndFrom
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK())) // acknowledge message received

        b match { // Check if the body is any of the following classes, and execute program
          case api.getUnificationModelsInfo(dummy) =>
            spHeader = SPHeader(from = api.service, to = api.topicResponse, reply = api.service)
            sendAnswer(SPMessage.makeJson(spHeader, api.sendUnificationModelInfo(mName, mTags) ))

          case api.getUnificationModel(modelName) =>
            if(modelName == mName) {
              spHeader = SPHeader(from = api.service, to = api.service, reply = api.service)
              publish(api.topicRequest, SPMessage.makeJson(spHeader, api.sendUnificationModel(mName, mResources, mSetupRunner )))
            }
          case x =>
            }
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))
      }
  }
  def sendAnswer(mess: String) = publish(api.topicResponse, mess)
}



