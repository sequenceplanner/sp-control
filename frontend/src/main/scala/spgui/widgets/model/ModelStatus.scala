package spgui.widgets.model

import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.internal.StyleA
import sp.domain._
import spgui.availablemodelscircuit._

object ModelStatus {
  import spgui.widgets.model.{ModelStatusCSS => css}

  def render(props: ModelProxy[ModelsCircuitState]): VdomElement = {
    val text = props.value.activeModel.flatMap(_.info).map(_.name).getOrElse("None")
    <.li(
      <.div(
        use(css.container),
        <.i(use(css.icon), ^.className := "fa fa-cubes"),
        <.div(
          use(css.innerContainer),
          <.p(use(css.smallText), "Active model"),
          <.p(use(css.normalText), text)
        )
      )
    )
  }

  def use(s: StyleA): TagMod = ^.className := s.htmlClass

  private val component = ScalaComponent.builder[ModelProxy[ModelsCircuitState]]("ModelStatusWidget")
    .render_P(render)
    .build

  private def connectCircuit = ModelsCircuit.connect(state => state)

  def apply() = connectCircuit(component(_))


}
