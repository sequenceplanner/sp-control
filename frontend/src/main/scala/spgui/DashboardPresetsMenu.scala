package spgui

import diode.Action
import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import spgui.PresetNameModal.DashboardPresets
import spgui.circuit._
import spgui.components.{Icon, SPNavbarElements, SPNavbarElementsCSS}
import spgui.modal.{ModalResult, SimpleModal}
import spgui.theming.Theming.SPStyleSheet

import scalacss.DevDefaults._

/**
  * Created by alfredbjork on 2018-03-26.
  */

object DashboardPresetsCSS extends SPStyleSheet {
  import dsl._

  val closeIcon = style(
    marginLeft(6.px),
    opacity(0),
    float.right,
    color(_rgb(theme.value.navbarBackgroundColor)),
    &.hover(
      color(_rgb(theme.value.navbarButtonTextColor))
    )
  )

  val menuItem = style(
  )

  val menuUl = style(
    unsafeChild("." + menuItem.htmlClass + ":hover ." + closeIcon.htmlClass)(
      opacity(1)
    )
  )

  this.addToDocument()
}

object DashboardPresetsMenu {

  case class ProxyContents(presets: DashboardPresets, widgets: OpenWidgets, widgetData: WidgetData)

  type ProxyType = ModelProxy[ProxyContents]

  case class Props(
                    proxy: ProxyType,
                    onDidMount: () => Unit = () => Unit
                  )


  class Backend($: BackendScope[Props, Unit]) {

    private def openSaveModal(p: Props) = {
      p.proxy.dispatchCB(
        OpenModal(
          "Save preset",
          close => PresetNameModal(getSelectedPreset(dashboardPresets(p), openWidgets(p)).getOrElse(""), close),
          { case PresetNameModal.Return(name: String) => p.proxy.dispatchCB(saveCurrentLayout(name)) }
        )
      )
    }

    private def saveCurrentLayout(name: String) = AddDashboardPreset(name)

    private def deleteLayout(name: String) = RemoveDashboardPreset(name)

    private def recallLayout(p: DashboardPreset) = RecallDashboardPreset(p)

    private def dashboardPresets(p: Props): Map[String, DashboardPreset] = p.proxy.modelReader.value.presets.xs

    private def openWidgets(p: Props): OpenWidgets = p.proxy.modelReader.value.widgets

    private def presetIsSelected(preset: DashboardPreset, openWidgets: OpenWidgets) = preset.widgets.equals(openWidgets)

    private def getSelectedPreset(presets: DashboardPresets, openWidgets: OpenWidgets): Option[String] =
      getSelectedPreset(presets.xs, openWidgets)

    private def getSelectedPreset(presets: Map[String, DashboardPreset], openWidgets: OpenWidgets): Option[String] =
      presets.flatMap(
        {case (name, preset) => if (openWidgets.equals(preset.widgets)) Some(name) else None}
      ).headOption

    private def clickIsOnDeleteButton(e: ReactEventFromHtml): Boolean = {
      val deleteClassName = DashboardPresetsCSS.closeIcon.htmlClass
      e.target.classList.contains(deleteClassName) || e.target.parentElement.classList.contains(deleteClassName)
    }

    def render(p: Props) = {
      def dispatch(action: Action): Callback = p.proxy.dispatchCB(action)

      SPNavbarElements.dropdown(
        "Layout",
        Seq(
          TagMod(^.className := DashboardPresetsCSS.menuUl.htmlClass),
          SPNavbarElements.dropdownElement(
            "Save layout",
            openSaveModal(p)
          )
        ) ++
          dashboardPresets(p).map {
            case (name, preset) =>

              val icon = if (presetIsSelected(preset, openWidgets(p))) Icon.dotCircleO else Icon.circle

              SPNavbarElements.dropdownElement(
                Seq(
                  TagMod(^.className := DashboardPresetsCSS.menuItem.htmlClass),
                  TagMod(^.onClick ==> (
                    e => {
                      if (clickIsOnDeleteButton(e))
                        Callback(SimpleModal.open("Delete " + "\"" + name + "\"?", dispatch(deleteLayout(name))))
                      else
                        dispatch(recallLayout(preset))
                    }
                    )),
                  <.span(icon, ^.className := SPNavbarElementsCSS.textIconClearance.htmlClass),
                  <.span(name),
                  <.span(Icon.times, ^.className := DashboardPresetsCSS.closeIcon.htmlClass)
                ).toTagMod
              )
          }
      )
    }

  }

  private val component = ScalaComponent.builder[Props]("DashboardPresetsMenu")
    //.initialState(State(""))
    .renderBackend[Backend]
    .componentDidMount(ctx => Callback(ctx.props.onDidMount))
    //.componentWillUnmount(_.backend.willUnmount())
    .build

  def apply(proxy: ProxyType, onDidMount: () => Unit = () => Unit): VdomElement =
    component(Props(proxy, onDidMount))
}

object PresetNameModal {
  case class Props(
                    close: Return => Callback,
                    placeholderText: String = ""
                  )

  case class State(
                    textBoxContent: String = ""
                  )

  case class Return(name: String) extends ModalResult

  class Backend($: BackendScope[Props, State]) {

    def render(props: Props, state: State) = {
      <.div(
        SPNavbarElements.TextBox(
          state.textBoxContent,
          "Preset name",
          i => $.modState(s => s.copy(textBoxContent = i))
        ),
        <.button(
          ^.onClick ==> (_ => props.close(Return(state.textBoxContent))),
          "Save"
        )
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("PresetNameModal")
    .initialStateFromProps(p => State(p.placeholderText))
    .renderBackend[Backend]
    .build

  def apply(placeholderText: String = "", close: Return => Callback): VdomElement = component(Props(close, placeholderText))

  case class DashboardPresets(xs: Map[String, DashboardPreset] = Map())
}