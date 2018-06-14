package spgui.widgets.examples

import diode.react.{ModelProxy, ReactConnectProxy}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import sp.domain._
import spgui.communication._
import sp.domain.Logic._
import monocle.macros._
import org.scalajs.dom.window
import spgui.SimpleSet
import spgui.availablemodelscircuit._
import spgui.widgets.examples.{ModelsWidgetNewCSS => css}




object ModelsWidgetNew {
  val testModel: List[IDAble] = {
    val operations = (1 to 4).map(x => Operation(s"o$x"))
    val things = (1 to 4).map(x => Thing(s"t$x"))
    val firstSop = SOPSpec("sop1", List(Sequence(operations.take(3).map(SOP(_)).toList)))
    val secondSop = SOPSpec("sop2", List(Parallel(operations.take(3).map(SOP(_)).toList)))


    val operationNodes = operations.map(o => StructNode(o.id))
    val thingNodes = things.map(t => StructNode(t.id))
    val sop1S = StructNode(firstSop.id)
    val sop2S = StructNode(secondSop.id)

    val struct1 = Struct("struct1", Set(
      thingNodes.head,
      thingNodes.tail.head,
      operationNodes.head.copy(parent = Some(thingNodes.head.nodeID)),
      operationNodes.tail.head.copy(parent = Some(thingNodes.tail.head.nodeID)),
      thingNodes.drop(2).head.copy(parent = Some(thingNodes.head.nodeID)),
      operationNodes.drop(2).head.copy(parent = Some(thingNodes.drop(2).head.nodeID)),
      sop1S.copy(parent = Some(operationNodes.drop(2).head.nodeID))
    ))

    // always unique node IDs in every struct!
    val o1S2 = StructNode(operations.head.id)
    val struct2 = Struct("struct2", Set(
      o1S2,
      thingNodes.last.copy(parent = Some(o1S2.nodeID)),
      operationNodes.last,
      sop2S.copy(parent = Some(operationNodes.last.nodeID))
    ))

    (operations ++ things ++ List(firstSop, secondSop, struct1, struct2)).toList
  }

  import sp.models.{APIModelMaker => ModelMaker}
  import sp.models.{APIModel => Model}
  import spgui.SPMessageUtil.BetterSPMessage

  @Lenses case class UIState(historyExpanded: Set[ID], shownIdables: List[IDAble])
  @Lenses case class State(uiState: UIState)

  case class Props(proxy: ModelProxy[ModelsCircuitState]) {
    def models: SimpleSet[ID, ModelMock] = proxy.value.models
    def activeModel: Option[ModelMock] = proxy.value.activeModel
    def activeModelId: Option[ID] = proxy.value.activeModelId
    def dispatch(action: diode.Action): Callback = proxy.dispatchCB(action)
    def dispatchAction[A](modifier: A => A, wrapper: A => diode.Action)(maybeA: Option[A]): Callback = {
      maybeA.map(a => dispatch(wrapper(modifier(a)))).getOrElse(Callback.empty)
    }
  }


  private class Backend($: BackendScope[Props, State]) {
    private val observerTopic = ModelMaker.topicResponse
    private val websocketObserver = BackendCommunication.getWebSocketStatusObserver(
      mess => if (mess) sendToHandler(ModelMaker.GetModels),
      observerTopic
    )
    private val topicHandler = BackendCommunication.getMessageObserver(message => handleMess(message), observerTopic)
    val idAblesLens: Lens[State, List[IDAble]] = State.uiState composeLens UIState.shownIdables

    def onModelResponse(props: Props, res: Model.Response): Callback = {
      res match {
        case Model.SPItems(items) =>
          $.modState(idAblesLens.set(items))

        case _ => Callback.empty
      }
    }

    def onModelMakerResponse(props: Props, res: ModelMaker.Response): Callback = {
      res match {
        case ModelMaker.ModelList(modelIds) =>
          modelIds.foreach { m =>
            sendToModel(m, Model.GetModelInfo)
            sendToModel(m, Model.GetModelHistory)
          }

          props.dispatch(AddMockModels(modelIds.map(id => ModelMock(id))))

        case created: ModelMaker.ModelCreated =>
          //sendToModel(modelId, Model.PutItems(TestModelInControl.getTestModel))
          props.dispatch(AddMockModels(ModelMock(created.id)))

        case ModelMaker.ModelDeleted(modelId) =>
          props.dispatch(RemoveModel(modelId))

        case _ => Callback.empty
      }
    }

    def handleMess(message: SPMessage): Unit = {
      val callback = $.props >>= { props =>
        val result = for (res <- message.oneOf[ModelMaker.Response].or[Model.Response].body) yield res match {
          case res: ModelMaker.Response => onModelMakerResponse(props, res)
          case res: Model.Response => onModelResponse(props, res)
          case _ => Callback.empty
        }
        result.getOrElse(Callback.empty)
      }

      callback.runNow()
    }

    def btn(title: String, onClick: Callback, icon: String, text: String = "", small: Boolean = true): TagMod = {
      btnWithTags(title, onClick, icon, text, small)()
    }
    def btnWithTags(title: String, onClick: Callback, icon: String, text: String = "", small: Boolean = true)(mods: TagMod*): TagMod = {
      <.button(
        ^.className := s"btn ${if (small) "btn-sm" else "btn-default"}",
        (^.title := title).when(title != ""),
        ^.onClick --> onClick,
        <.i(^.className := icon),
        text.when(text != ""),
        mods.toTagMod
      )
    }

    def renderModels(props: Props, state: State): TagMod = {
      val models = props.models.map(_.id).toList.sorted

      <.table(
        ^.className := "table table-striped",
        <.caption("Models"),
        <.thead(<.tr(
          <.th("id"),
          <.th("name"),
          <.th("version"),
          <.th("number of items"),
          <.th("Selected"),
          <.th("rename"),
          <.th("put dummy items"),
          <.th("preview"),
          <.th("export"),
          <.th("delete")
        )),
        <.tbody(
          models.flatMap(modelId => renderModel(props, state, modelId)).toTagMod
        )
      )
    }

    def renderModel(props: Props, state: State, modelId: ID) = {

      val historyExpanded = State.uiState ^|-> UIState.historyExpanded

      val isActiveModel = props.activeModelId.contains(modelId)

      def infoValue[T](f: Model.ModelInformation => T, default: T) = {
        props.models.get(modelId).flatMap(_.info).map(f).getOrElse(default)
      }

      val expandedButton = {
        val expanded = state.uiState.historyExpanded.contains(modelId)
        val onClick: Callback = $.modState(historyExpanded.modify { xs =>
          if (expanded) xs - modelId
          else xs + modelId
        })
        val icon: String = if (expanded) "fa fa-chevron-up" else "fa fa-chevron-down"

        btn("", onClick, icon)
      }

      val buttonStyle = {
        if (isActiveModel) css.activeModelButton
        else css.inactiveModelButton
      }

      List(
        <.tr(
          <.td(
            expandedButton,
            s" ${modelId.toString}"
          ),
          <.td(infoValue(_.name, "")),
          <.td(infoValue(_.version, -1).toString),
          <.td(infoValue(_.noOfItems, -1).toString),
          <.td(
            btnWithTags("Set as active model", onSetActive(props, modelId), if (isActiveModel) "fa fa-circle" else "fa fa-circle-thin")(
              ^.className := buttonStyle.htmlClass
            )
          ),
          <.td(btn("Change name of model", onChangeName(props, modelId), "fa fa-pencil")),
          <.td(btn("Add some dummy items", onAddDummyItems(props, modelId), "fa fa-bolt")),
          <.td(btn("Preview model", onPreviewModel(props, modelId), "fa fa-eye")),
          <.td(btn("Export model", onExportModel(props, modelId), "fa fa-print")),
          <.td(btn("Delete model", onDeleteModel(props, modelId), "fa fa-trash"))
        ),
        <.tr(
          <.td(
            ^.colSpan := 42,
            renderHistoryTable(props, state, modelId)
          )
        ).when(state.uiState.historyExpanded.contains(modelId))
      )
    }

    def changeNamePrompt(currentName: String): CallbackTo[String] = CallbackTo[String] {
      window.prompt("Name your model", currentName)
    }

    def onChangeName(props: Props, modelId: ID): Callback = {
      val currentName = props.models.get(modelId).flatMap(_.info.map(_.name)).getOrElse("")
      changeNamePrompt(currentName) >>= { name =>
        val circuitCallback = props.models.get(modelId).map { model =>
          props.dispatch(UpdateModel(ModelMock.info.modify(_.map(_.copy(name = name)))(model)))
        }.getOrElse(Callback.empty)

        circuitCallback >> sendToModel(modelId, Model.UpdateModelAttributes(Some(name), None))
      }
    }

    def onSetActive(props: Props, modelId: ID): Callback = {
      props.proxy.dispatchCB(SetActiveModel(modelId))
    }

    def onPreviewModel(props: Props, modelId: ID): Callback = {
      sendToModel(modelId, Model.GetItemList())
    }

    def onExportModel(props: Props, modelId: ID): Callback = {
      sendToModel(modelId, Model.ExportModel)
    }

    def onDeleteModel(props: Props, modelId: ID): Callback = props.proxy.dispatchCB(RemoveModel(modelId))

    def onRevertVersion(props: Props, modelId: ID, key: Int): Callback = {
      sendToModel(modelId, Model.RevertModel(key))
    }

    def onRefreshModels(): Callback = {
      sendToHandler(ModelMaker.GetModels)
    }

    def onAddDummyItems(props: Props, modelId: ID): Callback = {
      sendToModel(modelId, Model.PutItems(testModel))
    }

    def onCreateModel(): Callback = {
      sendToHandler(ModelMaker.CreateModel("testmodel"))
    }

    def renderHistoryTable(props: Props, state: State, modelId: ID): TagMod = {
      val history = props.models.get(modelId)
        .flatMap(_.history)
        .map(_.history)
        .getOrElse(List())

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
                btn(s"Revert to version $key", onRevertVersion(props, modelId, key), "fa fa-undo")
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

    def render(props: Props, state: State): VdomElement = {
      <.div(
        btn("", onCreateModel(), "fa fa-bolt", "Create test model", small = false),
        btn("", onRefreshModels(), "fa fa-refresh", "Refresh models", small = false),
        renderModels(props, state),
        renderModelPreview(state),
        ^.className := css.container.htmlClass
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
  val initialState: State = State(initialUIState)

  private val component = ScalaComponent.builder[Props]("ModelsWidget")
    .initialState(initialState)
    .renderBackend[Backend]
    .componentDidMount(_.backend.onMount())
    .componentWillUnmount(_.backend.onUnmount())
    .build

  val connectCircuit: ReactConnectProxy[ModelsCircuitState] = ModelsCircuit.connect(state => state)

  def apply() = spgui.SPWidget(_ => connectCircuit { proxy => component(Props(proxy)) })
}