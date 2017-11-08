package spgui.widgets.modelImport

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import org.scalajs.dom.html

import play.api.libs.json.Json
import spgui.communication._

import sp.domain._
import Logic._

import sp.modelImport.{APISPModelImport => api} //Create short API names
import sp.models.{APIModelMaker => mmapi}
import sp.models.{APIModel => mapi}
import api.{ImportText, ModelInfo}

object SPModelImportWidget {

  private class Backend($: BackendScope[Unit, ImportText]) {

    val messObs = BackendCommunication.getMessageObserver(
      mess => {
        import api.Formats._
        val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[api.Model].map {
              case api.Model(modelID: ID, info: ModelInfo, ids: List[IDAble]) =>
                sendToHandler(mmapi.CreateModel(info.name, info.attributes, modelID)) // Create model
                sendToModel(modelID, mapi.PutItems(ids)) // Populate model with IDAbles
                sendToHandler(mmapi.GetModels) // Refresh model
              case x => Callback.empty
        }
        callback.foreach(_.runNow())
      },
      api.topicResponse // Listen to Model Import API
    )

    def render(s: ImportText) = {
      <.div(
        <.button(
          ^.className := "btn btn-default",
          ^.onClick --> sendToModelImport(ImportText(s.Text)), "Import model"
        ),
        <.button(
          ^.className := "btn btn-default",
          ^.onClick  --> prettify(s), "Prettify text"
        ),
        <.br(),
        <.textarea(
          ^.id := "txtarea",
          ^.tpe := "text",
          ^.rows := 20, // initial size of textarea
          ^.cols := 75,
          ^.defaultValue := s.Text,
          ^.onChange ==> onTextChange // update var text with new text
        )
      )
    }

    def prettify(s: ImportText) ={
      val newValue = Json prettyPrint (Json parse s.Text) // formats the text to be more appealing
      dom.document.getElementById("txtarea").asInstanceOf[html.Input].value = newValue // Get the textarea value and update it
      $.modState(_.copy(Text = newValue)) // Update the state of the text, might be redundant. since the textarea should update on change..
    }
    def onTextChange(e: ReactEventFromTextArea) = {
      val newValue = e.target.value // Get the modified value from the text area
      $.modState(_.copy(Text = newValue))// Update the state of the text
    }


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

    def onUnmount() = {
      println("Unmounting")
      messObs.kill()
      Callback.empty
    }
  }

  private val component = ScalaComponent.builder[Unit]("ExampleServiceWidget")
    .initialState(ImportText("Input SP model as Json string here"))
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(spwb => component())
}
