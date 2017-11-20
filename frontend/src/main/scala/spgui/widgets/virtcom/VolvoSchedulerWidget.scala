package spgui.widgets.virtcom

import java.util.UUID

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^.{<, _}
import org.scalajs.dom
import org.scalajs.dom.html
import play.api.libs.json.Json
import sp.domain._
import sp.virtcom.{APIVolvoScheduler => api}
import sp.virtcom.APIVolvoScheduler.{calculate, generateSOPs, getCases}
import sp.models.{APIModel => mapi, APIModelMaker => mmapi}
import spgui.communication._
import spgui.components.{Icon, SPWidgetElements}

object VolvoSchedulerWidget {

   case class State(modelID : String, selectedIDs : Set[ID], idables : List[IDAble], selectedIdables : List[IDAble], sopId : ID, cases : Map[String, List[Operation]], neglectedCases : Set[ID])
  var modelId =""

  private class Backend($: BackendScope[Unit, State]) {
    val messObs = BackendCommunication.getMessageObserver(
      mess => {
        val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[mapi.Response].map {
          case mapi.SPItems(items) =>
            $.modState(_.copy(idables = items , modelID = modelId))
          case x => Callback.empty
        }
        callback.foreach(_.runNow())
      },
      mapi.topicResponse
    )

    val messObs2 = BackendCommunication.getMessageObserver(
      mess => {
        val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[api.Response].map {
          case api.gotCases(c) =>
            $.modState(_.copy(cases = c))
          case x => Callback.empty
        }
        callback.foreach(_.runNow())
      },
      api.topicResponse
    )


    def render(s: State) = {
      <.div(
        renderInput(s),
        renderSelected(s),

        <.br(),
        <.button(
          ^.className := "btn btn-default",
          ^.onClick --> sendToVolvoScheduler(generateSOPs(s.modelID , s.selectedIDs , s.idables)),
          "Generate SOPs"
        ),
        <.button(
          ^.className := "btn btn-default",
          ^.onClick --> sendToVolvoScheduler(getCases(s.sopId , s.idables)),
          "Get Cases"
        ),
        <.button(
          ^.className := "btn btn-default",
          ^.onClick  --> sendToVolvoScheduler(calculate(s.sopId , s.idables, s.neglectedCases)),
          "Synthesize & solve"
        )
      ,
        renderCases(s)
      )
    }


    def renderSelected(s: State) = {
      <.table(
        ^.className := "table table-striped",
        <.caption("Selected "),
        <.thead(<.tr(
          <.th("Type"),
          <.th("Name"),
          <.th("ID"),
          <.th("Remove")
        )),
        <.tbody(
          s.selectedIdables.map(i =>
            <.tr(
              <.td(i.getClass.getSimpleName),
              <.td(i.name),
              <.td(i.id.toString),
              <.td(<.button(^.className := "btn btn-sm",
                ^.onClick --> removeItem(i,s),
                <.i(^.className := "fa fa-trash")
              )
              )
            )).toTagMod
        )).when(s.selectedIdables.nonEmpty)
    }

    def renderInput(s :State)  ={
      <.div(
      <.p("input model ID: "),
      <.input(
        ^.tpe := "text",
        ^.onChange ==> onModelChange
      ),
      <.p("input item ID: "),
      <.input(
        ^.id := "SelectID",
        ^.tpe := "text",
        ^.onChange --> addItem(s)
      ),
        <.p("SOP ID: "),
        <.input(
          ^.tpe := "text",
          ^.onChange ==> onSopIdChange
        )
      )
    }

    def renderCases(s: State) = {
      s.cases.map(c=>
        SPWidgetElements.dropdown(
          c._1,
          c._2.map(o =>
            SPWidgetElements.dropdownElement(
              o.name,
              {if(!s.neglectedCases.contains(o.id)) Icon.checkSquare else Icon.square},
              onCaseCheck(o.id, s)
            )
          )
        )
      ).toTagMod
    }

    def onCaseCheck(opId :ID, s : State) = {
      if(s.neglectedCases.contains(opId))
        $.modState(_.copy(neglectedCases = s.neglectedCases - opId))
      else
        $.modState(_.copy(neglectedCases = s.neglectedCases + opId))
    }

    def onModelChange(e: ReactEventFromInput) = {
      modelId = e.target.value.replaceAll("\\s", "") // Get the modified value from the text area, remove white spaces
      sendToModel(java.util.UUID.fromString(modelId), mapi.GetItemList(0,99999))
    }

    def addItem(s : State) = {
      val newIDset = s.selectedIDs + java.util.UUID.fromString(dom.document.getElementById("SelectID").asInstanceOf[html.Input].value) // Get the textarea value and update it
      dom.document.getElementById("SelectID").asInstanceOf[html.Input].value = ""
      val newSelectedIdables = s.idables.filter(idable => newIDset.contains(idable.id))
      $.modState(_.copy( selectedIdables = newSelectedIdables, selectedIDs = newIDset ))
    }

    def removeItem(idable: IDAble, s : State) = {
      val newIDset = s.selectedIDs - idable.id
      val newSelectedIdables = s.idables.filter(idable => newIDset.contains(idable.id))
      $.modState(_.copy( selectedIdables = newSelectedIdables, selectedIDs = newIDset ))
    }

    def onSopIdChange(e: ReactEventFromInput) = {
      val newValue = e.target.value.replaceAll("\\s", "") // Get the modified value from the text area, remove white spaces
      $.modState(_.copy(sopId = java.util.UUID.fromString(newValue)))
    }

    def sendToModel(model: ID, mess: mapi.Request): Callback = {
      val h = SPHeader(from = "VolvoSchedulerWidget", to = model.toString,
        reply = SPValue("VolvoSchedulerWidget"))
      val json = SPMessage.make(h, mess)
      BackendCommunication.publish(json, mapi.topicRequest)
      Callback.empty
    }
    def sendToHandler(mess: mmapi.Request): Callback = {
      val h = SPHeader(from = "VolvoSchedulerWidget", to = mmapi.service,
        reply = SPValue("VolvoSchedulerWidget"))
      val json = SPMessage.make(h, mess)
      BackendCommunication.publish(json, mmapi.topicRequest)
      Callback.empty
    }
    def sendToVolvoScheduler(mess: api.Request): Callback = {
      val h = SPHeader(from = "VolvoSchedulerWidget", to = api.service, reply = SPValue("VolvoSchedulerWidget"))
      val json = SPMessage.make(h, mess)
      BackendCommunication.publish(json, api.topicRequest)
      Callback.empty
    }

    def onUnmount() = {
      println("Unmounting")
      messObs.kill()
      messObs2.kill()
      Callback.empty
    }
  }


  private val component = ScalaComponent.builder[Unit]("VolvoSchedulerWidget")
    .initialState(State("", Set(), List(),List(), ID.newID, Map(), Set()))
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(spwb => component())
}
