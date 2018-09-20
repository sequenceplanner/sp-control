package spgui.widgets.model

import diode.react.{ModelProxy, ReactConnectProxy}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros._
import sp.domain.Logic._
import sp.domain._
import sp.models.APIModel
import spgui.circuits.availablemodelscircuit._
import spgui.circuit.{OpenModal, SPGUICircuit}
import spgui.modal.ModalResult
import spgui.SimpleSet
import spgui.circuits.main.handlers._
import spgui.circuits.main.MainCircuit
import spgui.communication.ModelCommunication

object ModelsWidget {
  import sp.models.{APIModel => Model, APIModelMaker => ModelMaker}
  import spgui.widgets.model.{ModelsWidgetCSS => css}

  @Lenses case class UIState(historyExpanded: Set[ID], selectedModelId: Option[ID])
  @Lenses case class State(uiState: UIState)

  case class Props(proxy: ModelProxy[ModelHandlerState]) {
    def models: SimpleSet[ID, ModelMock] = proxy.value.models
    def activeModel: Option[ModelMock] = proxy.value.activeModel
    def activeModelId: Option[ID] = proxy.value.activeModelId
    def dispatch(action: diode.Action): Callback = proxy.dispatchCB(action)
    def dispatchAction[A](modifier: A => A, wrapper: A => diode.Action)(maybeA: Option[A]): Callback = {
      maybeA.fold(Callback.empty)(a => dispatch(wrapper(modifier(a))))
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
        //<.caption("Models"),
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
          <.td(btn("Change name of model", showChangeNameModal(props, modelId), "fa fa-pencil")),
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
      //println(props.activeModel.map(_.items))
      <.div(
        ^.className := css.container.htmlClass,
        renderModels(props, state),
        renderModelPreview(props, state),
        btnWithTags("", onCreateModel(), "fa fa-bolt", " Create test model", small = false)(^.className := css.mainButton.htmlClass),
        btnWithTags("", onRefreshModels(), "fa fa-refresh", " Refresh models", small = false)(^.className := css.mainButton.htmlClass)
      )
    }

    def modalComponent(name: String): (ModalResult => Callback) => VdomElement = {
      def modal(onClose: ModalResult => Callback) = RenameModal(onClose, name = name).vdomElement

      modal
    }

    def onChangeModelName(props: Props, modelId: ID, newName: String): Callback = {
      val circuitCallback = props.models.get(modelId).map { model =>
        props.dispatch(UpdateModel(ModelMock.info.modify(_.map(_.copy(name = newName)))(model)))
      }.getOrElse(Callback.empty)

      circuitCallback >> Callback {
        ModelCommunication.postRequest(modelId, Model.UpdateModelAttributes(Some(newName), None))
      }
    }

    def showChangeNameModal(props: Props, modelId: ID): Callback = Callback {
      val currentName = for {
        model <- props.models.get(modelId)
        info <- model.info
      } yield info.name

      SPGUICircuit.dispatch(OpenModal(
        title = "Rename",
        component = modalComponent(currentName.getOrElse("")),
        onComplete = {
          case RenameModal.Return(submitted, name) =>
            if (submitted) onChangeModelName(props, modelId, name)
            else Callback.empty
        }
      ))
    }

    def onSetActive(props: Props, modelId: ID): Callback = {
      props.proxy.dispatchCB(SetActiveModel(modelId)) >> Callback {
        ModelCommunication.postRequest(modelId, APIModel.GetItemList(0,99999))
      }
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
      println("onCreateModel")
      ModelCommunication.postRequest(ModelMaker.CreateModel("Test Model"))
    }

    def onUnmount(): Callback = Callback.empty


    def onMount() =  Callback{
      ModelCommunication.postRequest(ModelMaker.GetModels)
    }
  }

  val initialUIState: UIState = UIState(Set(), None)
  val initialState: State = State(initialUIState)

  private val component = ScalaComponent.builder[Props]("ModelsWidget")
    .initialState(initialState)
    .renderBackend[Backend]
    .componentDidMount(_.backend.onMount())
    .componentWillUnmount(_.backend.onUnmount())
    .build

  val connectCircuit: ReactConnectProxy[ModelHandlerState] = MainCircuit.connect(_.models)

  def apply() = spgui.SPWidget(_ => connectCircuit { proxy => component(Props(proxy)) })

  val testModel: List[IDAble] = {
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
