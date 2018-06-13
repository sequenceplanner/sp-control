package spgui.widgets.examples

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import sp.domain._
import spgui.communication._
import sp.domain.Logic._

import monocle.macros._
import monocle.Lens

import org.scalajs.dom.window


object TestModelInControl {
  def getTestModel: List[IDAble] = {
    val o1 = Operation("o1")
    val o2 = Operation("o2")
    val o3 = Operation("o3")
    val o4 = Operation("o4")
    val t1 = Thing("t1")
    val t2 = Thing("t2")
    val t3 = Thing("t3")
    val t4 = Thing("t4")
    val sop1 = SOPSpec("sop1", List(Sequence(List(SOP(o1), SOP(o2), SOP(o3)))))
    val sop2 = SOPSpec("sop2", List(Parallel(List(SOP(o1), SOP(o2), SOP(o3)))))

    // TODO: Fix a good struct DSL now!
    val o1S = StructNode(o1.id)
    val o2S = StructNode(o2.id)
    val o3S = StructNode(o3.id)
    val o4S = StructNode(o4.id)
    val t1S = StructNode(t1.id)
    val t2S = StructNode(t2.id)
    val t3S = StructNode(t3.id)
    val t4S = StructNode(t4.id)
    val sop1S = StructNode(sop1.id)
    val sop2S = StructNode(sop2.id)

    val struct1 = Struct("struct1", Set(
      t1S,
      t2S,
      o1S.copy(parent = Some(t1S.nodeID)),
      o2S.copy(parent = Some(t2S.nodeID)),
      t3S.copy(parent = Some(t1S.nodeID)),
      o3S.copy(parent = Some(t3S.nodeID)),
      sop1S.copy(parent = Some(o3S.nodeID))
    ))

    // always unique node IDs in every struct!
    val o1S2 = StructNode(o1.id)
    val struct2 = Struct("struct2", Set(
      o1S2,
      t4S.copy(parent = Some(o1S2.nodeID)),
      o4S,
      sop2S.copy(parent = Some(o4S.nodeID))
    ))

    List(o1, o2, o3, o4, t1, t2, t3, t4, sop1, sop2, struct1, struct2)
  }
}

object ModelsWidget {
  import sp.models.{APIModelMaker => ModelMaker}
  import sp.models.{APIModel => Model}

  implicit class SPMessageInto(message: SPMessage) {
    def to[T](implicit reads: JSReads[T]): Option[(SPHeader, T)] = for {
      h <- message.getHeaderAs[SPHeader]
      b <- message.getBodyAs[T]
    } yield (h, b)
  }


  @Lenses case class UIState(historyExpanded: Set[ID], shownIdables: List[IDAble])
  @Lenses case class ModelState(models: Set[ID], modelInfo: Map[ID, Model.ModelInformation], modelHistory: Map[ID, Model.ModelHistory])
  @Lenses case class State(modelState: ModelState, uiState: UIState)

  private class Backend($: BackendScope[Unit, State]) {
    private val observerTopic = ModelMaker.topicResponse
    private val websocketObserver = BackendCommunication.getWebSocketStatusObserver(
      mess => if (mess) sendToHandler(ModelMaker.GetModels),
      observerTopic
    )
    private val topicHandler = BackendCommunication.getMessageObserver(handleMess, observerTopic)

    def modelLens[T](lens: Lens[ModelState, T]): Lens[State, T] = State.modelState composeLens lens

    def uiLens[T](lens: Lens[UIState, T]): Lens[State, T] = State.uiState composeLens lens

    def parseBackendMessage(message: SPMessage): Option[(SPHeader, Any)] = {
      message.to[ModelMaker.Response] match {
        case None => message.to[Model.Response]
        case res => res
      }
    }

    def onModelResponse(res: Model.Response): Callback = res match {
      case info: Model.ModelInformation =>
        $.modState(modelLens(ModelState.modelInfo).modify(_ + (info.id -> info)))

      case history @ Model.ModelHistory(id, _) =>
        $.modState(modelLens(ModelState.modelHistory).modify(_ + (id -> history)))

      case Model.ModelUpdate(modelId, version, count, _, _, _) =>
        sendToModel(modelId, Model.GetModelHistory)

        $.modState(modelLens(ModelState.modelInfo).modify { modelInfo =>
          val newEntry = modelInfo.get(modelId).map { info =>
            modelId -> Model.ModelInformation(info.name, info.id, version, count, info.attributes)
          }

          modelInfo ++ newEntry
        })

      case Model.SPItems(items) =>
        $.modState(uiLens(UIState.shownIdables).set(items))

      case _ => Callback.empty
    }

    def onModelMakerResponse(res: ModelMaker.Response): Callback = res match {
      case ModelMaker.ModelList(models) =>
        models.foreach { m =>
          sendToModel(m, Model.GetModelInfo)
          sendToModel(m, Model.GetModelHistory)
        }

        $.modState(modelLens(ModelState.models).set(models.toSet))

      case created: ModelMaker.ModelCreated =>
        //sendToModel(modelId, Model.PutItems(TestModelInControl.getTestModel))
        $.modState(modelLens(ModelState.models).modify(_ + created.id))

      case ModelMaker.ModelDeleted(modelId) =>
        $.modState(modelLens(ModelState.models).modify(_.filterNot(_ == modelId)))

      case _ => Callback.empty
    }

    def handleMess(message: SPMessage): Unit = {
      println(s"handleMess()")
      for ((_, res) <- parseBackendMessage(message)) {
        val callback = res match {
          case res: ModelMaker.Response => onModelMakerResponse(res)
          case res: Model.Response => onModelResponse(res)
          case _ => Callback.empty
        }

        callback.runNow()
      }
    }


    def renderModels(s: State): TagMod = {
      <.table(
        ^.className := "table table-striped",
        <.caption("Models"),
        <.thead(<.tr(
          <.th("id"),
          <.th("name"),
          <.th("version"),
          <.th("number of items"),
          <.th("rename"),
          <.th("put dummy items"),
          <.th("preview"),
          <.th("export"),
          <.th("delete")
        )),
        <.tbody(
          s.modelState.models.toList.sorted.flatMap(modelId => {
            def modelValue[T](f: Model.ModelInformation => T, default: T) = {
              s.modelState.modelInfo.get(modelId).map(f).getOrElse(default)
            }

            List(
              <.tr(
                <.td(
                  if (s.uiState.historyExpanded.contains(modelId))
                    <.button(^.className := "btn btn-sm",
                      ^.onClick --> $.modState(uiLens(UIState.historyExpanded).modify(_ - modelId)),
                      <.i(^.className := "fa fa-chevron-up")
                    )
                  else
                    <.button(^.className := "btn btn-sm",
                      ^.onClick --> $.modState(uiLens(UIState.historyExpanded).modify(_ + modelId)),
                      <.i(^.className := "fa fa-chevron-down")
                    ),
                  " " + modelId.toString
                ),
                <.td(modelValue(_.name, "")),
                <.td(modelValue(_.version, -1).toString),
                <.td(modelValue(_.noOfItems, -1).toString),
                <.td(
                  <.button(
                    ^.className := "btn btn-sm",
                    ^.title := "Change name of model",
                    ^.onClick --> Callback {
                      val newName = window.prompt("Name your model", "My Model")
                      sendToModel(modelId, Model.UpdateModelAttributes(Some(newName), None)).runNow()
                    },
                    <.i(^.className := "fa fa-pencil")
                  )
                ),
                <.td(
                  <.button(
                    ^.className := "btn btn-sm",
                    ^.title := "Add some dummy items",
                    ^.onClick --> sendToModel(modelId, Model.PutItems(TestModelInControl.getTestModel)),
                    <.i(^.className := "fa fa-bolt")
                  )
                ),
                <.td(
                  <.button(
                    ^.className := "btn btn-sm",
                    ^.title := "Preview model",
                    ^.onClick --> sendToModel(modelId, Model.GetItemList()),
                    <.i(^.className := "fa fa-eye")
                  )
                ),
                <.td(
                  <.button(
                    ^.className := "btn btn-sm",
                    ^.title := "Export model",
                    ^.onClick --> sendToModel(modelId, Model.ExportModel),
                    <.i(^.className := "fa fa-print")
                  )
                ),
                <.td(
                  <.button(
                    ^.className := "btn btn-sm",
                    ^.title := "Delete model",
                    ^.onClick --> sendToHandler(ModelMaker.DeleteModel(modelId)),
                    <.i(^.className := "fa fa-trash")
                  )
                )
              ),
              <.tr(<.td(^.colSpan := 42, renderHistoryTable(s, modelId))).when(s.uiState.historyExpanded.contains(modelId))
            )
          }).toTagMod
        )
      )
    }

    def renderHistoryTable(s: State, modelId: ID): TagMod = {
      val history = s.modelState.modelHistory.get(modelId).map(_.history).getOrElse(List())
      <.table(
        ^.className := "table table-striped",
        <.caption("History"),
        <.thead(<.tr(
          <.th("Version"),
          <.th("Info"),
          <.th("Revert")
        )),
        <.tbody(
          history.map { case (key, attributes) =>
            <.tr(
              <.td(key),
              <.td(attributes.getAs[String]("info").getOrElse("no info").toString),
              <.td(
                <.button(
                  ^.className := "btn btn-sm",
                  ^.title := ("Revert to version " + key),
                  ^.onClick --> sendToModel(modelId, Model.RevertModel(key)),
                  <.i(^.className := "fa fa-undo")
                )
              )
            )
          }.toTagMod
        ))
    }

    def renderModelPreview(s: State): TagMod = {
      <.table(
        ^.className := "table table-striped",
        <.caption("Model Preview"),
        <.thead(<.tr(
          <.th("Type"),
          <.th("Name"),
          <.th("ID")
        )),
        <.tbody(
          s.uiState.shownIdables.map(i =>
            <.tr(
              <.td(i.getClass.getSimpleName),
              <.td(i.name),
              <.td(i.id.toString)
            )).toTagMod
        )).when(s.uiState.shownIdables.nonEmpty)
    }

    def render(state: State): VdomElement = {
      <.div(
        <.button(
          ^.className := "btn btn-default",
          ^.onClick --> sendToHandler(ModelMaker.CreateModel("testmodel")), <.i(^.className := "fa fa-bolt"), " Create test model"
        ),
        <.button(
          ^.className := "btn btn-default",
          ^.onClick --> sendToHandler(ModelMaker.GetModels), <.i(^.className := "fa fa-refresh"), " Refresh models"
        ),
        renderModels(state),
        renderModelPreview(state)
      )
    }

    def sendToHandler(mess: ModelMaker.Request): Callback = Callback {
      val h = SPHeader(
        from = "ModelWidget",
        to = ModelMaker.service,
        reply = SPValue("ModelWidget")
      )
      val json = SPMessage.make(h, mess)
      BackendCommunication.publish(json, Model.topicRequest)
    }

    def sendToModel(model: ID, mess: Model.Request): Callback = Callback {
      val h = SPHeader(
        from = "ModelWidget",
        to = model.toString,
        reply = SPValue("ModelWidget")
      )
      val json = SPMessage.make(h, mess)
      BackendCommunication.publish(json, Model.topicRequest)
    }

    def onUnmount() = Callback {
      topicHandler.kill()
      websocketObserver.kill()
    }

    def onMount(): Callback = Callback.empty
  }

  val initialUIState: UIState = UIState(Set(), shownIdables = List())
  val initialModelState: ModelState = ModelState(models = Set(), modelInfo = Map(), modelHistory = Map())
  val initialState: State = State(initialModelState, initialUIState)

  private val component = ScalaComponent.builder[Unit]("ModelsWidget")
    .initialState(initialState)
    .renderBackend[Backend]
    .componentDidMount(_.backend.onMount())
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(spwb => component())
}