package spgui.widgets.modelImport

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import org.scalajs.dom.html
import play.api.libs.json.Json
import spgui.communication._
import sp.domain._
import sp.modelImport.{APISPModelImport => api}
import sp.models.{APIModelMaker => mmapi}
import sp.models.{APIModel => mapi}
import api.{ImportText, ModelInfo}
import spgui.components.Icon

object SPModelImportWidget {

  case class State(Text : String, randID : Boolean)

  private class Backend($: BackendScope[Unit, State]) {

    val messObs = BackendCommunication.getMessageObserver(
      mess => {
        import api.Formats._
        val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[api.Model].map {
              case api.Model(modelID: ID, info: ModelInfo, ids: List[IDAble]) =>
                val mID = if($.state.runNow().randID) ID.newID else modelID
                sendToHandler(mmapi.CreateModel(info.name, info.attributes, mID)) // Create model
                sendToModel(mID, mapi.PutItems(ids)) // Populate model with IDAbles
                sendToHandler(mmapi.GetModels) // Refresh model
              case x => Callback.empty
        }
        callback.foreach(_.runNow())
      },
      api.topicResponse // Listen to Model Import API
    )

    def render(s: State) = {
      <.div(
        <.button(
          ^.className := "btn btn-default",
          ^.onClick --> sendToModelImport(ImportText(s.Text)), "Import model"
        ),
        <.button(
          ^.className := "btn btn-default",
          ^.onClick  --> prettify(s), "Expand"
        ),
        <.button(
          ^.className := "btn btn-default",
          ^.onClick  --> stringify(s), "Compact"
        ),
        <.div(if(s.randID) Icon.checkSquare else Icon.square , "Randomize the model ID",^.onClick --> $.modState(_.copy(randID = !s.randID))),
        <.br(),
        <.textarea(
          ^.tpe := "text", ^.rows := 20, ^.cols := 75,
          ^.value := s.Text,
          ^.onClick --> (if(s.Text == "Input SP model as Json string here") $.modState(_.copy(Text = "")) else $.modState(_.copy(Text = s.Text))),
          ^.onChange ==> onTextChange // update var text with new text
        )
      )
    }

    def prettify(s: State) ={
      val newValue = Json prettyPrint (Json parse s.Text) // formats the text to be more appealing
      //dom.document.getElementById("txtarea").asInstanceOf[html.Input].value = newValue // Get the textarea value and update it
      $.modState(_.copy(Text = newValue)) // Update the state of the text, might be redundant. since the textarea should update on change..
    }

    def stringify(s: State) ={
      val newValue = Json stringify (Json parse s.Text) // formats the text to be more appealing
      //dom.document.getElementById("txtarea").asInstanceOf[html.Input].value = newValue // Get the textarea value and update it
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
    .initialState(State("Input SP model as Json string here", true))
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(spwb => component())
}
