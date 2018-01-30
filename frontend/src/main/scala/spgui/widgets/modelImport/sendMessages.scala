package spgui.widgets.modelImport

import japgolly.scalajs.react.Callback
import sp.domain.{ID, SPHeader, SPMessage, SPValue}
import sp.models.{APIModel => mapi, APIModelMaker => mmapi}
import sp.modelImport.{APISPModelImport => api}

import spgui.communication.BackendCommunication

object sendMessages {

  def sendToModelImport(mess: api.Request): Callback = {
    val h = SPHeader(from = "SPModelImportWidget", to = api.service, reply = SPValue("SPModelImportWidget"))
    val json = SPMessage.make(h, mess)
    BackendCommunication.publish(json, api.topicRequest)
    Callback.empty
  }
  def sendToHandler(mess: mmapi.Request): Callback = {
    val h = SPHeader(from = "ModelWidget", to = mmapi.service,
      reply = SPValue("ModelWidget"))
    val json = SPMessage.make(h, mess)
    BackendCommunication.publish(json, mmapi.topicRequest)
    Callback.empty
  }
  def sendToModel(model: ID, mess: mapi.Request): Callback = {
    val h = SPHeader(from = "ModelWidget", to = model.toString,
      reply = SPValue("ModelWidget"))
    val json = SPMessage.make(h, mess)
    BackendCommunication.publish(json, mapi.topicRequest)
    Callback.empty
  }


}
