package spgui.communication

import sp.domain.SPMessage
import spgui.SPMessageUtil.BetterSPMessage
import spgui.circuits.main.handlers.{RunnerHandler, RunnerAction, ModelNames}
import spgui.circuits.main.FrontendState

object MiniModelHelperCommunication extends CommunicationAPI.Communicator[String, RunnerAction] {
  import sp.modelSupport.{APIMiniModelService => API}
  val responseTopic: String = API.topicResponse

  def onReceiveMessage(message: SPMessage): Unit = {
    val response = message.as[API.Response]

    for ((_, body) <- response) body match {
      case API.sendModelsInfo(modelNames) =>
        localDispatch(ModelNames(modelNames))

    }
  }

  def postRequest(request: API.Request): Unit = {
    post(
      request,
      from = "MiniModelCommunication",
      to = API.service,
      topic = API.topicRequest
    )
  }

  override protected def stateAccessFunction: FrontendState => String = NoState

  override def defaultReply: String = "MiniModelCommunication"
}
