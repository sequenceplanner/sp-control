package spgui.widgets.virtcom

import japgolly.scalajs.react.Callback
import sp.domain.{ID, SPHeader, SPMessage, SPValue}

import spgui.communication.BackendCommunication

import sp.virtcom.{APIBDDVerifier, APIVolvoScheduler => api}
import sp.models.{APIModel => mapi, APIModelMaker => mmapi}
import spgui.widgets.ganttviewer.APIGantt.{APIGanttViewer => apiGantt}

object sendMessages {

  def sendToModel(model: ID, mess: mapi.Request): Callback = { //  Send message to model
    val h = SPHeader(from = "VolvoSchedulerWidget", to = model.toString,
      reply = SPValue("VolvoSchedulerWidget"))
    val json = SPMessage.make(h, mess)
    BackendCommunication.publish(json, mapi.topicRequest)
    Callback.empty
  }

  def sendToHandler(mess: mmapi.Request): Callback = { // Send message to model handler
    val h = SPHeader(from = "VolvoSchedulerWidget", to = mmapi.service,
      reply = SPValue("VolvoSchedulerWidget"))
    val json = SPMessage.make(h, mess)
    BackendCommunication.publish(json, mmapi.topicRequest)
    Callback.empty
  }

  def sendToVolvoScheduler(mess: api.Request): Callback = { // Send message to Volvo scheduler service
    val h = SPHeader(from = "VolvoSchedulerWidget", to = api.service, reply = SPValue("VolvoSchedulerWidget"))
    val json = SPMessage.make(h, mess)
    BackendCommunication.publish(json, api.topicRequest)
    Callback.empty
  }

  def sendToBDDVerifier(mess: APIBDDVerifier.Request): Callback = { // Send message to BDDVerifier
    val h = SPHeader(from = "VolvoSchedulerWidget", to = APIBDDVerifier.service, reply = SPValue("VolvoSchedulerWidget"))
    val json = SPMessage.make(h, mess)
    BackendCommunication.publish(json, APIBDDVerifier.topicRequest)
    Callback.empty
  }

  def sendToGanttViewer(mess: apiGantt.Response): Callback = { // Send message to BDDVerifier
    val h = SPHeader(from = "VolvoSchedulerWidget", to = "GanttViewerWidget", reply = SPValue("VolvoSchedulerWidget"))
    val json = SPMessage.make(h, mess)
    BackendCommunication.publish(json, apiGantt.topicResponse)
    Callback.empty
  }


}
