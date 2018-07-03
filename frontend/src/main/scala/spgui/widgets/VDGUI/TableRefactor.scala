package spgui.widgets.VDGUI

import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import spgui.widgets.VDGUI.CSSHelpers.toHtml
import spgui.widgets.VDGUI.{TableCSSRedesign => css}

object TableRefactor {
  case class ColumnData(label: String, style: TagMod = css.none)

  case class Props[A <: Product](columnData: Vector[ColumnData], rows: Iterable[A], onClick: Callback)

  private class Backend[A <: Product]($: BackendScope[Props[A], Option[Int]]) {

    def render(props: Props[A]): VdomElement = {
      <.table(
        css.table,
        ^.onClick --> props.onClick,
        <.thead(
          <.tr(
            css.row,
            props.columnData.map(d => <.th(css.headerCell, d.style, d.label)).toTagMod
          )
        ),
        <.tbody(
          props.rows.map(renderRow).toTagMod
        )
      ).render
    }

    def renderRow(row: A): TagMod = {
      val values: List[TagMod] = row.productIterator.toList.map {
        case v: VdomElement => v
        case v => TagMod(v.toString)
      }

      val cells = values.map(v => <.td(css.dataCell, v))

      <.tr(css.row, cells.toTagMod)
    }

  }

  private def component[A <: Product] = ScalaComponent.builder[Props[A]]("Table")
    .initialState(None: Option[Int])
    .renderBackend[Backend[A]]
    .build

  def apply[A <: Product](headers: Vector[ColumnData], rows: Iterable[A], onClick: Callback = Callback.empty): VdomElement = {
    component[A](Props(headers, rows, onClick))
  }
}
