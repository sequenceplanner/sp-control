package spgui.widgets.virtcom

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^.{<, _}
import org.scalajs.dom
import org.scalajs.dom.html
import sp.domain._
import sp.domain.Logic._
import sp.virtcom.{APIBDDVerifier, APIVolvoScheduler => api}
import sp.virtcom.APIVolvoScheduler.{calculate, generateSOPs, getCases}
import sp.models.{APIModel => mapi, APIModelMaker => mmapi}
import spgui.communication._
import spgui.components.{Icon, SPWidgetElements}


// Todo: integrate with item explorer, SOP maker and gantt viewer.
// Todo: Create collapsible panels or similar feature

object VolvoSchedulerWidget{

  case class State(modelID : String, selectedIDs : Set[ID], idables : List[IDAble], selectedIdables : List[IDAble], sopId : ID, cases : Map[String, List[Operation]], neglectedCases : Set[ID],structId : ID, selectedThingDs: Map[String,(String,Int)], verificationResult : Int, doneCalculating :Boolean, cpResults : SPAttributes)
  var modelId ="" // should not be necessary

  private class Backend($: BackendScope[Unit, State]) {

    val modelMessObs = BackendCommunication.getMessageObserver( // update idables and model ID
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

    val volvoschedulerMessObs = BackendCommunication.getMessageObserver(
      mess => {
        val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[api.Response].map {

          case api.gotCases(c) => // Update cases
            $.modState(_.copy(cases = c))

          case api.generatedSopID(id)  => // when the sops are generated, get the main sop id, refresh the model and get Cases
            sendToModel(java.util.UUID.fromString(modelId), mapi.GetItemList(0,99999))
            dom.document.getElementById("SopIdInput").asInstanceOf[html.Input].value = id.toString // update textbox
            $.modState(_.copy(sopId = id))

          case api.calculateStructID(id)  => // when the calculations are done, refresh model and save the new struct id
            sendToModel(java.util.UUID.fromString(modelId), mapi.GetItemList(0,99999))
            $.modState(_.copy(structId = id, doneCalculating = true))

          case api.cpResults(cpRes)  => // Save optimization results
            $.modState(_.copy(cpResults = cpRes))

          case x => Callback.empty
        }
        callback.foreach(_.runNow())
      },
      api.topicResponse
    )

    val bddVerifierMessObs = BackendCommunication.getMessageObserver( // Receives the BDD verification result and updates the state
      mess => {
        val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIBDDVerifier.Response].map {
          case APIBDDVerifier.VerificationResult(res) =>
            $.modState(_.copy(verificationResult = res.compare(false)))
          case x => Callback.empty
        }
        callback.foreach(_.runNow())
      },
      APIBDDVerifier.topicResponse
    )

    def render(s: State) = { // render GUI
      <.div(
        renderInput(s),
        renderSelected(s),

        if(s.cases.nonEmpty) {<.div(<.br(), <.p("Cases active during optimization") )}else <.p(""),

        renderCases(s),
        renderButtons(s),
        renderCpRes(s),
        renderVerification(s)
      )
    }


    def renderButtons(s: State) ={
      <.div(
        <.br(), <.br(), <.br(),

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
          ^.onClick  --> sendToVolvoScheduler(calculate(s.modelID, s.sopId , s.idables, s.neglectedCases)),
          "Synthesize & solve"
        )
      )
    }

    def renderSelected(s: State) = { // Show the selected IDAbles in a table, with additional naming for robot schedules
      <.div(
      if(s.selectedIdables.nonEmpty) <.p("Selected items from the model") else <.p("Select items from the model"),
      <.table(
        ^.className := "table table-striped",
        ^.id := "selected-items",
        <.tbody(
          s.selectedIdables.map(i =>{
            <.tr(
              <.td(i.name),
              <.td(getRobotName(s: State, i :IDAble)),
              <.td(<.button(^.className := "btn btn-sm",
                ^.onClick --> removeItem(i,s),
                <.i(^.className := "fa fa-trash")
              )
              )
            )}).toTagMod
        )).when(s.selectedIdables.nonEmpty)
      )
    }


    def renderInput(s :State)  ={ // This is for preliminary testing of the program, giving inputs as text
      <.div(
        <.table(
          ^.className := "table table-striped",
          <.thead(
            <.th("input model ID"),
            <.th("input item ID"),
            <.th("SOP ID")
          ),
          <.tbody(
              <.tr(
                <.td(<.input(^.tpe := "text",^.onChange ==> onModelChange)),
                <.td(<.input( ^.id := "SelectID", ^.tpe := "text", ^.onChange --> addItem(s))),
                <.td(<.input( ^.id := "SopIdInput", ^.tpe := "text", ^.onChange ==> onSopIdChange))
            )
          )
        ),
        <.br()
      )
    }

    def renderCases(s: State) = { // Create drop down menus for the cases, it is possible to have multiple cases selected
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

    def renderThings(s : State) = { // Create drop down menus for the Things, only one thing should be selected at a time. Todo: modify to get rid of push button for verification
      val activeStruct = s.idables.filter(_.isInstanceOf[Struct]).map(_.asInstanceOf[Struct]).find(_.id == s.structId).getOrElse(Struct(""))
      val idsInStruct = s.idables.filter(i => activeStruct.items.map(_.item).contains(i.id))
      idsInStruct.filter(_.isInstanceOf[Thing]).map(_.asInstanceOf[Thing])  // Get the active struct, find idables in struct and Things in idables,
        .map(thing =>
          SPWidgetElements.dropdown(
            thing.name,
            thing.attributes.getAs[SPAttributes]("stateVariable").getOrElse(SPAttributes()).getAs[List[String]]("domain").getOrElse(List(""))
              .zipWithIndex.map( di =>
              SPWidgetElements.dropdownElement(
                di._1,
                {if(s.selectedThingDs.get(thing.name).getOrElse(("",0))._1.equals(di._1)) Icon.checkSquare else Icon.square},
                onThingCheck(s, thing.name,di)
              )
            )
          )
        ).toTagMod
    }


    def renderCpRes(s :State)  ={ // display results of CP in paragraphs and table
     val numStates = s.cpResults.getAs[Int]("numStates").getOrElse(0)
      val cpCompl = s.cpResults.getAs[Boolean]("cpCompleted").getOrElse(false)
      val cpSops = s.cpResults.getAs[List[(Double,SOPSpec, List[(ID,Double,Double)])]]("cpSops").getOrElse(List())
      val bddName = s.cpResults.getAs[String]("bddName").getOrElse("")
      val cpTime = s.cpResults.getAs[Long]("cpTime").getOrElse(0)

      <.div(
        <.br(),
        <.p("Number of states in supervisor:  ", numStates),
        <.p("Constraint programming complete:  ",  cpCompl.toString),
        <.p("Time to find CP solution:  ", cpTime.toString, " ms"),
        <.br(),
      <.table(
        ^.className := "table table-striped",
        <.thead(
          <.th("Execution time of solutions"),
          <.th(""),
          <.th("")
        ),
        <.tbody(
          cpSops.map(res =>{
            <.tr(
              <.td(res._1, " s"),
              <.td(<.button(^.className := "btn btn-sm"/*,^.onClick --> openSOP( res._2) */,"Open SOP")), // Todo: send to SOP maker and Gantt viewer
                <.td(<.button(^.className := "btn btn-sm"/*,^.onClick --> openSOP( res._2) */,"Open Gantt"))
              )}
              ).toTagMod
            ))
      ).when(cpSops.nonEmpty)
    }

    def renderVerification(s: State) ={ // show BDD verification interface and result
      <.div(
        <.br(),
        if(s.doneCalculating){ // only show the verification part of GUI once calculation is done
          <.div(
            <.p("BDD Verification :   ", if(s.verificationResult ==1) "This can happen" else if(s.verificationResult ==0) "This cannot happen" else ""),
            renderThings(s),
            <.button( // Test if the selected state is achievable
              ^.className := "btn btn-default",
              ^.onClick  --> sendToBDDVerifier(APIBDDVerifier.VerifyBDD("dummy",s.selectedThingDs.map(m => (m._1, m._2._2)))),
              "Verify"
            )
          )
        }
        else <.p("")
      )
    }

    // For dropdown menus, on click
    def onCaseCheck(opId :ID, s : State) = {
      if(s.neglectedCases.contains(opId))
        $.modState(_.copy(neglectedCases = s.neglectedCases - opId))
      else
        $.modState(_.copy(neglectedCases = s.neglectedCases + opId))
    }
    def onThingCheck(s : State, tName : String, di : (String,Int)) = {
      if(s.selectedThingDs.get(tName).getOrElse(("",0))._1.equals(di._1)) {
        $.modState(_.copy(selectedThingDs = (s.selectedThingDs ++ Map(tName -> ("", 0)))))
      }
      else {
        $.modState(_.copy(selectedThingDs = (s.selectedThingDs ++ Map(tName -> di))))
      }
    }

    def onModelChange(e: ReactEventFromInput) = { // Update model when text area changes
      modelId = e.target.value.replaceAll("\\s", "") // Get the modified value from the text area, remove white spaces
      sendToModel(java.util.UUID.fromString(modelId), mapi.GetItemList(0,99999))
    }


    def addItem(s : State) = {  // Add item to selection
      val newIDset = s.selectedIDs + java.util.UUID.fromString(dom.document.getElementById("SelectID").asInstanceOf[html.Input].value) // Get the textarea value and update it
      dom.document.getElementById("SelectID").asInstanceOf[html.Input].value = ""
      val newSelectedIdables = s.idables.filter(idable => newIDset.contains(idable.id))
      $.modState(_.copy( selectedIdables = newSelectedIdables, selectedIDs = newIDset ))
    }
    def removeItem(idable: IDAble, s : State) = { // Remove item from selection
      val newIDset = s.selectedIDs - idable.id
      val newSelectedIdables = s.idables.filter(idable => newIDset.contains(idable.id))
      $.modState(_.copy( selectedIdables = newSelectedIdables, selectedIDs = newIDset ))
    }

    def onSopIdChange(e: ReactEventFromInput) = { // update the state of sopId
      val newValue = e.target.value.replaceAll("\\s", "") // Get the modified value from the text area, remove white spaces
      $.modState(_.copy(sopId = java.util.UUID.fromString(newValue)))
    }
    def getRobotName(s: State, i : IDAble ) ={
      if(i.attributes.getAs[List[String]]("robotcommands").getOrElse(List()).nonEmpty) { // If the IDAble i is a Robot, get the struct containing i, find the corresponding structnode of i and get the parent node's name, i.e robot name.
        val activeStruct = s.idables.filter(_.isInstanceOf[Struct]).map(_.asInstanceOf[Struct]).find(struct => struct.items.map(_.item).contains(i.id)).get
        val parentStructNodeItem = activeStruct.items.find(_.nodeID == activeStruct.items.find(sn => sn.item == i.id).get.parent.get).get.item
        s.idables.find(_.id == parentStructNodeItem).get.name
      }
      else
        "" // dont write anything extra, for now... Would be cool to get the other resource type and such here, like fixture and so on
    }

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

    def onUnmount() = { // Unmounting widget
      println("Unmounting")
      modelMessObs.kill()
      volvoschedulerMessObs.kill()
      bddVerifierMessObs.kill()
      Callback.empty
    }
  }

  private val component = ScalaComponent.builder[Unit]("VolvoSchedulerWidget")
    .initialState(State("", Set(), List(),List(), ID.newID, Map(), Set(), ID.newID, Map(), -1, false, SPAttributes()))
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(spwb => component())

}


