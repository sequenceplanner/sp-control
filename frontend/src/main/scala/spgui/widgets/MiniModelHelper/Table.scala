package spgui.widgets.VDGUI

import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import spgui.widgets.VDGUI.CSSHelpers.toHtml
import spgui.widgets.VDGUI.{TableCSS => css}

object Table {
  case class ColumnData(label: String, style: TagMod = css.none)

  case class Props[A <: Product](columnData: Vector[ColumnData], rows: Iterable[A], onClick: Callback)

  private class Backend[A <: Product]($: BackendScope[Props[A], Option[Int]]) {
    def makeColumns(props: Props[A], hoveredRow: Option[Int]): TagMod = {
      val columnInfo = rowsToColumns(props)
        .filter { case (_, v) => v.nonEmpty }
        .flatMap { case (i, columnValues) => props.columnData.lift(i).map(columnData => (columnData, columnValues)) }

      columnInfo.map { case (columnData, columnValues) =>
        val columns = renderColumnData(columnValues, hoveredRow)

        <.div(
          columnData.style,
          css.flexChild,
          <.div(^.className := "table-header", css.data, columnData.label),
        columns
        )
      }.toTagMod
    }

    def render(props: Props[A], hoveredRow: Option[Int]): VdomElement = {
      val columns = makeColumns(props, hoveredRow)

      <.div(
        ^.onClick --> props.onClick,
        css.flexParent,
        columns
      )
    }

    def renderColumnData(columnCells: Vector[Any], hoveredRow: Option[Int]): TagMod = columnCells.zipWithIndex.map {
      case (cell: VdomElement, index) =>
        <.div(
          css.data,
          cell
        )

      case (cell, index) =>
        <.div(
          ^.className := "table-data",
          css.data,
          cell.toString
        )
    }.toTagMod

    def rowsToColumns(props: Props[A]): Map[Int, Vector[Any]] = {
      props.rows.toList.foldLeft(Map[Int, Vector[Any]]()) { (columns, next) =>
        next.productIterator.zipWithIndex
          .filter { case (_, i) => i < props.columnData.size }
          .foldLeft(columns) { case (columnData, (v, i)) =>
            val curr = columnData.getOrElse(i, Vector())
            columnData.updated(i, curr :+ v)
          }
      }
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
