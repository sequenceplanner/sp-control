package spgui.widgets.virtcom

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import org.scalajs.dom.html

import sp.domain._
import sp.domain.Logic._

import scala.collection.immutable.ListMap
import scala.collection.mutable

import spgui.communication._

import sp.virtcom.{APIBDDVerifier, APIVolvoScheduler => api}
import sp.virtcom.APIVolvoScheduler.{calculate, generateSOPs, getCases}
import sp.models.{APIModel => mapi}

import spgui.components.Icon

import spgui.widgets.itemexplorerincontrol.ModelChoiceDropdown
import sendMessages._
import dropdownWithScroll.dropdownWScroll
import openGantt.showGantt

// Todo: integrate with item explorer, SOP maker and gantt viewer.

object VolvoSchedulerWidget{
  case class State(modelID : ID, selectedIDs : Set[ID], idables : List[IDAble], selectedIdables : List[IDAble], sopId : ID, cases : Map[String, List[Operation]], neglectedCases : Set[ID],structId : ID, selectedThingDs: Map[String,(String,Int)], verificationResult : Int, doneCalculating :Boolean, cpResults : SPAttributes)
  var modelId = ID.newID // should not be necessary

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

    val volvoschedulerMessObs = BackendCommunication.getMessageObserver( //listen to messages from Volvo Scheduler
      mess => {
        val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[api.Response].map {

          case api.gotCases(c) => // Update cases
            val elem = dom.document.getElementById("lngCase")
            val lngstStr = if(c.nonEmpty) c.keySet.maxBy(_.length) else "" //longest case name
            elem.textContent =lngstStr
            ShowBusy(false)
            $.modState(_.copy(cases = c))

          case api.generatedSopID(id)  => // when the sops are generated, get the main sop id, refresh the model and get Cases
            sendToModel(modelId, mapi.GetItemList(0,99999))
            ShowBusy(false)
            $.modState(_.copy(sopId = id))

          case api.calculateStructID(id)  => // when the calculations are done, refresh model and save the new struct id
            sendToModel(modelId, mapi.GetItemList(0,99999))
            $.modState(_.copy(structId = id, doneCalculating = true))

          case api.cpResults(cpRes)  => // Save optimization results
            ShowBusy(false)
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
        <.div(
          renderInputsAndButtons(s), // menu
          renderSelected(s), // selected schedules
          renderCases(s), // render alternatives where the SOPs are branching
          renderRes(s), // render results of synthesis and Constraint Programming
          renderVerification(s) // For BDD verification of Supervisor
        ),
        <.div(^.id := "VolvoSchedulerSpinner", ^.className := Style.spinLoader.htmlClass), // a loader that can show when the widget is busy
        <.div(^.id :="lngCase", ^.className := Style.lngCaseHide.htmlClass), // this is a hidden div for calculating pixel size of the longest case.
      )
    }

    def renderInputsAndButtons(s: State) ={
      <.div(
        ^.className := Style.inputs.htmlClass,
        ModelChoiceDropdown(id => {modelId = id; sendToModel(id, mapi.GetItemList(0,99999))}), // Get all models in dropdown
        renderRobotSchOps(s), // Get robot schedules in dropdown
        <.button(
          ^.className := "btn", ^.className := Style.buttons.htmlClass,
          ^.onClick --> {ShowBusy(); sendToVolvoScheduler(generateSOPs(s.modelID , s.selectedIDs , s.idables))}, // Generate SOPs
          "Generate SOPs"
        ),
        <.button(
          ^.className := "btn", ^.className :=  Style.buttons.htmlClass,
          ^.onClick --> {ShowBusy(); sendToVolvoScheduler(getCases(s.sopId , s.idables))}, // get cases
          "Get Cases"
        ),
        <.button(
          ^.className := "btn", ^.className :=  Style.buttons.htmlClass,
          ^.onClick  --> {ShowBusy();  sendToVolvoScheduler(calculate(s.modelID, s.sopId , s.idables, s.neglectedCases))}, // Synthesise and use CP to solve model
          "Synthesize & solve"
        )
      )
    }

    def renderSelected(s: State) =  // Show the selected IDAbles in a table, with additional naming for robot schedules

      <.details( ^.open := "open", ^.className := Style.collapsible.htmlClass,
        <.summary("Selected resources/schedules"),
      <.table(
        ^.className := "table table-striped", ^.className :="Table",
        ^.id := "selected-items",
        <.tbody(
          s.selectedIdables.map(i =>{
            <.tr(
              <.td(getRobotName(s: State, i :IDAble)), // find name of parent operation to selected schedule (should be the robot name)
              <.td(i.name),
              <.td(<.button(^.className := "btn btn-sm",
                ^.onClick --> removeItem(i,s),
                <.i(^.className := "fa fa-trash")
              )
              )
            )}).toTagMod
        )), <.br(),
    ).when(s.selectedIdables.nonEmpty)

    def renderCases(s: State) = {
      var tRows = mutable.LinkedHashSet[TagOf[html.TableRow]]() // for saving row items in the table
      if(s.cases.nonEmpty) {

        val sortedCases = ListMap(s.cases.toSeq.sortBy(_._1): _*).toList // Sort the cases by name
        val elem = dom.document.getElementById("lngCase") // get longest case name element and get pixel width to use for all cases.
        val caseWidthPx = if (elem.clientWidth <= 500) (elem.clientWidth +50).toString + "px" else "500px"

        for (List(c1, c2) <- sortedCases.grouped(2)) { // take 2 cases at the time and create dropdown menus, side by side
          val contents1 = c1._2.map(o => <.div(if (!s.neglectedCases.contains(o.id)) Icon.checkSquare else Icon.square, " " + o.name.substring(o.name.indexOf("_") + 1), ^.onClick --> onCaseCheck(o.id, s)))
          val contents2 = c2._2.map(o => <.div(if (!s.neglectedCases.contains(o.id)) Icon.checkSquare else Icon.square, " " + o.name.substring(o.name.indexOf("_") + 1), ^.onClick --> onCaseCheck(o.id, s)))
          val row = <.tr(<.td(dropdownWScroll(c1._1, contents1, caseWidthPx)),
            <.td(dropdownWScroll(c2._1, contents2, caseWidthPx)))
          tRows += row
        } // Todo: try sliding instead, perhaps no need for the if clause
        if (sortedCases.length % 2 != 0) { // uneven nbr of cases
          val c1 = sortedCases(sortedCases.length - 1)
          val contents1 = c1._2.map(o => <.div(if (!s.neglectedCases.contains(o.id)) Icon.checkSquare else Icon.square, " " + o.name.substring(o.name.indexOf("_") + 1), ^.onClick --> onCaseCheck(o.id, s)))
          val row = <.tr(<.td(dropdownWScroll(c1._1, contents1, caseWidthPx)))
          tRows += row
        }
      }
      <.details( ^.open := "open", ^.className := Style.collapsible.htmlClass,
        <.summary("Cases active during optimization"),
        <.table(^.className := "table table-striped", ^.className := "Table", <.tbody( // create table
          tRows.toTagMod))).when(s.cases.nonEmpty)
    }


    def renderRes(s :State)  ={ // display results of synthesis and CP in table
      val numStates = s.cpResults.getAs[Int]("numStates").getOrElse(0)
      val cpCompl = s.cpResults.getAs[Boolean]("cpCompleted").getOrElse(false)
      val cpSops = s.cpResults.getAs[List[(Double,SOPSpec, List[(ID,Double,Double)])]]("cpSops").getOrElse(List())
      val bddName = s.cpResults.getAs[String]("bddName").getOrElse("")
      val cpTime = s.cpResults.getAs[Long]("cpTime").getOrElse(0)

      <.details( ^.open := "open", ^.className := Style.collapsible.htmlClass,
        <.summary("Results"),
        <.br(),
        (numStates.toString + " states in supervisor," + "  constraint programming  " + (if(cpCompl) "completed in " + cpTime.toString + " ms" else "failed")),
        <.table(
          ^.className := "table table-striped",
          <.thead(
            <.tr(
              <.th("Execution time of solutions"),
            )
          ),
          <.tbody(
            cpSops.map(res =>{
              <.tr(
                <.td(res._1, " s"),
                <.td(<.button(^.className := "btn btn-sm"/*,^.onClick --> openSOP( res._2) */,"Open SOP")), // Todo: send to SOP maker and Gantt viewer
                <.td(<.button(^.className := "btn btn-sm",^.onClick --> showGantt( res._3, s.idables) ,"Open Gantt"))
              )}
            ).toTagMod
          )), <.br(),
      ).when(cpSops.nonEmpty)
    }


    def renderVerification(s: State) ={ // show BDD verification interface and result
      <.div(
        if(s.doneCalculating){ // only show the verification part of GUI once calculation is done
          <.details( ^.open := "open", ^.className := Style.collapsible.htmlClass,
            <.summary("BDD Verification"),
            <.p("Result :   ", if(s.verificationResult ==1) "This can happen" else if(s.verificationResult ==0) "This cannot happen" else ""),
            renderThings(s)
          )
        }
        else <.p("")
      )
    }

    def ShowBusy(show : Boolean = true)={ // show/hide loader spinner
      val spinner = dom.document.getElementById("VolvoSchedulerSpinner")
      if(show) spinner.setAttribute("style","display:block")
      else spinner.setAttribute("style","display:none")
    }


    def renderInput(s :State)  = // This is for preliminary testing of the program, at least the model part should be global
      <.div(^.className := Style.inputs.htmlClass,
        ModelChoiceDropdown(id => {modelId = id; sendToModel(id, mapi.GetItemList(0,99999))}),
        renderRobotSchOps(s)
      )

    def renderRobotSchOps(s: State) = <.div( // find idables with robot schedule and put them in a dropdown menu
      dropdownWScroll("Select Robot Schedules",
        s.idables.filter(_.isInstanceOf[Operation]).map(_.asInstanceOf[Operation]).filter(o=> {o.attributes.getAs[List[String]]("robotcommands").getOrElse(List()).nonEmpty && o.name.contains("SchDefault")} ).sortWith(_.name < _.name)
                    .map(op => <.div(op.name, "  " + getRobotName(s: State, op), ^.onClick --> addRobotSch(s, op.id), ^.className := Style.schSelect.htmlClass))))


    def onCaseCheck(opId :ID, s : State) = { // add or remove the selection
      if(s.neglectedCases.contains(opId))
        $.modState(_.copy(neglectedCases = s.neglectedCases - opId))
      else
        $.modState(_.copy(neglectedCases = s.neglectedCases + opId))
    }


    def renderThings(s: State) = {
      val activeStruct = s.idables.filter(_.isInstanceOf[Struct]).map(_.asInstanceOf[Struct]).find(_.id == s.structId).getOrElse(Struct(""))
      val idsInStruct = s.idables.filter(i => activeStruct.items.map(_.item).contains(i.id))
      val things = idsInStruct.filter(_.isInstanceOf[Thing]).map(_.asInstanceOf[Thing]).sortWith(_.name < _.name)
      things.map(thing => {

        val contents = thing.attributes.getAs[SPAttributes]("stateVariable").getOrElse(SPAttributes()).getAs[List[String]]("domain").getOrElse(List(""))
                        .zipWithIndex.map( di =>{
                                                val selected = s.selectedThingDs.get(thing.name).getOrElse(("",0))._1 == di._1; val domName = " " + di._1.replace(thing.name + "_","")
                                                <.div(if(selected)Icon.check else "  ", domName,  ^.onClick --> thingVerify(s, thing.name,di))})
        dropdownWScroll(thing.name, contents)
      }).toTagMod
    }

    def thingVerify(s : State, tName : String, di : (String,Int)) = {
        sendToBDDVerifier(APIBDDVerifier.VerifyBDD("dummy",(s.selectedThingDs ++ Map(tName -> di)).map(m => (m._1, m._2._2)))) // verify
        $.modState(_.copy(selectedThingDs = (s.selectedThingDs ++ Map(tName -> di)))) // update selected things
    }


    def onModelChange(e: ReactEventFromInput) = { // Update model when text area changes
      modelId = java.util.UUID.fromString(e.target.value.replaceAll("\\s", "")) // Get the modified value from the text area, remove white spaces
      sendToModel(modelId, mapi.GetItemList(0,99999))
    }

    def addRobotSch(s : State, id : ID) = {  // Add robot schedule item to selection
      val newIDset = s.selectedIDs + id // add id to set
      val newSelectedIdables = s.idables.filter(idable => newIDset.contains(idable.id)) // find IDAble
      $.modState(_.copy( selectedIdables = newSelectedIdables, selectedIDs = newIDset )) // update state
    }

    def removeItem(idable: IDAble, s : State) = { // Remove item from selection
      val newIDset = s.selectedIDs - idable.id
      val newSelectedIdables = s.idables.filter(idable => newIDset.contains(idable.id))
      $.modState(_.copy( selectedIdables = newSelectedIdables, selectedIDs = newIDset ))
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

    def onUnmount() = { // Unmounting widget
      println("Unmounting")
      modelMessObs.kill()
      volvoschedulerMessObs.kill()
      bddVerifierMessObs.kill()
      Callback.empty
    }
  }

  private val component = ScalaComponent.builder[Unit]("VolvoSchedulerWidget")
    .initialState(State(ID.newID, Set(), List(),List(), ID.newID, Map(), Set(), ID.newID, Map(), -1, false, SPAttributes()))
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(spwb => component())

}


