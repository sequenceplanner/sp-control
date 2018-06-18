package spgui.widgets.model

import diode.react.{ModelProxy, ReactConnectProxy}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros._
import org.scalajs.dom.window
import sp.domain.Logic._
import sp.domain._
import spgui.availablemodelscircuit._
import spgui.{ModelCommunication, SimpleSet}


object ModelsWidgetNew {
  import sp.models.{APIModel => Model, APIModelMaker => ModelMaker}
  import spgui.widgets.model.{ModelsWidgetNewCSS => css}

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

  @Lenses case class UIState(historyExpanded: Set[ID], selectedModelId: Option[ID])
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

    def renderModel(props: Props, state: State, modelId: ID): List[TagMod] = {
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

    def renderHistoryTable(props: Props, state: State, modelId: ID): TagMod = {
      val history = props.models.get(modelId)
        .flatMap(_.history)
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

    def renderModelPreview(props: Props, state: State): TagMod = {
      def renderIdRow(idAble: IDAble): TagMod = {
        <.tr(
          <.td(idAble.getClass.getSimpleName),
          <.td(idAble.name),
          <.td(idAble.id.toString)
        )
      }

      val model = state.uiState.selectedModelId.flatMap(modelId => props.models.get(modelId))

      <.table(
        ^.className := "table table-striped",
        <.caption("Model Preview"),
        <.thead(<.tr(
          <.th("Type"),
          <.th("Name"),
          <.th("ID")
        )),
        <.tbody(
          model.map(_.items).map(_.map(renderIdRow).toTagMod).whenDefined
        )
      )
    }

    def render(props: Props, state: State): VdomElement = {
      <.div(
        btnWithTags("", onCreateModel(), "fa fa-bolt", " Create test model", small = false)(^.className := css.mainButton.htmlClass),
        btnWithTags("", onRefreshModels(), "fa fa-refresh", " Refresh models", small = false)(^.className := css.mainButton.htmlClass),
        renderModels(props, state),
        renderModelPreview(props, state),
        ^.className := css.container.htmlClass
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

        circuitCallback >> Callback {
          ModelCommunication.postRequest(modelId, Model.UpdateModelAttributes(Some(name), None))
        }
      }
    }

    def onSetActive(props: Props, modelId: ID): Callback = {
      props.proxy.dispatchCB(SetActiveModel(modelId))
    }

    def onPreviewModel(props: Props, modelId: ID): Callback =  {
      $.modState((State.uiState ^|-> UIState.selectedModelId).set(Some(modelId))) >>
      Callback { ModelCommunication.postRequest(modelId, Model.GetItemList()) }
    }

    def onExportModel(props: Props, modelId: ID): Callback = Callback {
      ModelCommunication.postRequest(modelId, Model.ExportModel)
    }

    def onDeleteModel(props: Props, modelId: ID): Callback = props.proxy.dispatchCB(RemoveModel(modelId))

    def onRevertVersion(props: Props, modelId: ID, key: Int): Callback = Callback {
      ModelCommunication.postRequest(modelId, Model.RevertModel(key))
    }

    def onRefreshModels(): Callback = Callback {
      ModelCommunication.postRequest(ModelMaker.GetModels)
    }

    def onAddDummyItems(props: Props, modelId: ID): Callback = Callback {
      ModelCommunication.postRequest(modelId, Model.PutItems(testModel))
    }

    def onCreateModel(): Callback = Callback {
      ModelCommunication.postRequest(ModelMaker.CreateModel("Test Model"))
    }

    def onUnmount(): Callback = Callback.empty

    def onMount(): Callback = Callback.empty
  }

  val initialUIState: UIState = UIState(Set(), None)
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