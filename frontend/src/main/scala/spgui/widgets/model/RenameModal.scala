package spgui.widgets.model

import japgolly.scalajs.react.{BackendScope, Callback, ScalaComponent}
import spgui.modal.ModalResult
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.all.{button, onClick}
import scalacss.internal.StyleA
import vdom.html_<^._

object RenameModal {
  import spgui.widgets.model.{RenameModalCSS => css}

  def use(s: StyleA): TagMod = ^.className := s.htmlClass

  case class Props(close: Return => Callback, name: String)
  case class State(name: String)

  case class Return(submit: Boolean, name: String) extends ModalResult

  class Backend($: BackendScope[Props, State]) {
    private def onSubmit(state: State) = Return(submit = true, state.name)
    private def onCancel(state: State) = Return(submit = false, state.name)

    private def onNameChanged(event: ReactEventFromInput): Callback = {
      val value = event.target.value
      $.modState(_.copy(name = value))
    }

    private def onKeyPressed(props: Props, state: State, key: String): Callback = {
      val EnterKey = "Enter"

      key match {
        case EnterKey => props.close(onSubmit(state))
        case _ => Callback.empty
      }
    }

    def render(props: Props, state: State) = {
      <.div(
        use(css.container),
        <.p("Rename model"),
        <.input(
          ^.onChange ==> onNameChanged,
          ^.onKeyPress ==> (e => {
            val key = e.key
            onKeyPressed(props, state, key)
          }),
          ^.value := state.name),
        <.div(
          use(css.innerContainer),
          button(use(css.button), onClick --> props.close(onSubmit(state)), "Save name"),
          button(use(css.button), onClick --> props.close(onCancel(state)), "Cancel")
        )
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("RenameModal")
    .initialStateFromProps(p => State(p.name))
    .renderBackend[Backend]
    .build

  def apply(close: Return => Callback, name: String) =
    component(Props(close, name))
}