package spgui.widgets.modelImport

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import play.api.libs.json.Json
import spgui.communication._
import sp.domain._
import sp.modelImport.{APISPModelImport => api}
import sp.models.{APIModelMaker => mmapi}
import sp.models.{APIModel => mapi}
import api.{ImportText, ModelInfo}
import spgui.components.Icon
import  sendMessages._

object SPModelImportWidget {

  case class State(Text : String, randID : Boolean)

  private class Backend($: BackendScope[Unit, State]) {

    val importMessObs = BackendCommunication.getMessageObserver(
      mess => {
        import api.Formats.fModel
        import mapi.Formats.fModelToExport
        mess.getBodyAs[api.Model].map { // import model of the old variety
              case api.Model(modelID: ID, info: ModelInfo, ids: List[IDAble]) =>
                importModel(info.name, modelID, info.attributes, ids)
              case x => Callback.empty
        }
        mess.getBodyAs[mapi.ModelToExport].map { // import the new (previously exported) model
          case mapi.ModelToExport(name, id, version, attributes, items) =>
            importModel(name, id, attributes, items)
          case x => Callback.empty
        }
      },
      api.topicResponse // Listen to Model Import API
    )

    def importModel(name : String, id : ID, attributes : SPAttributes, items : List[IDAble]) ={
      val mID = if($.state.runNow().randID) ID.newID else id
      sendToHandler(mmapi.CreateModel(name, attributes, mID)) // Create model
      sendToModel(mID, mapi.PutItems(items)) // Populate model with IDAbles
      sendToHandler(mmapi.GetModels) // Refresh model
    }

    val exportMessObs = BackendCommunication.getMessageObserver(
      mess => {
        val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[mapi.Response].map {
          case m2e: mapi.ModelToExport =>
            $.modState(_.copy(Text = Json.toJson(m2e).toString))
          case x => Callback.empty
        }
        callback.foreach(_.runNow())
      },
      mapi.topicResponse
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
      $.modState(_.copy(Text = newValue)) // Update the state of the text, might be redundant. since the textarea should update on change..
    }

    def stringify(s: State) ={
      val newValue = Json stringify (Json parse s.Text) // formats the text to be more appealing
      $.modState(_.copy(Text = newValue)) // Update the state of the text, might be redundant. since the textarea should update on change..
    }

    def onTextChange(e: ReactEventFromTextArea) = {
      val newValue = e.target.value // Get the modified value from the text area
      $.modState(_.copy(Text = newValue))// Update the state of the text
    }


    def onUnmount() = {
      println("Unmounting")
      importMessObs.kill()
      exportMessObs.kill()
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
