package spgui.widgets.VDGUI

import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.internal.StyleA
import spgui.widgets.VDGUI.CSSHelpers.toHtml
import spgui.widgets.VDGUI.{TableCSS => css}

object Table {
  sealed trait Alignment
  object Alignment {
    case object Start extends Alignment
    case object Center extends Alignment
    case object End extends Alignment
  }


  case class ColumnData(label: String, style: StyleA = css.none)

  case class Props[A <: Product](headers: Vector[ColumnData], rows: Iterable[A], onClick: Callback)
  def makeColumns[A <: Product](props: Props[A]): TagMod = {
    val headerSize = props.headers.size

    val data = props.rows.toList.foldLeft(Map[Int, Vector[Any]]()) { (acc, next) =>
      next.productIterator.zipWithIndex
        .filter { case (_, i) => i < headerSize }
        .foldLeft(acc) { case (acc2, (v, i)) =>
          val curr = acc2.getOrElse(i, Vector())
          acc2.updated(i, curr :+ v)
      }
    }

    data
      .filter { case (_, v) => v.nonEmpty }
      .flatMap { case (i, columnValues) => props.headers.lift(i).map(header => (header, columnValues)) }
      .map { case (columnData, columnValues) =>
        val rows = columnValues.tail.map {
          case d: VdomElement => <.div(css.value, d)
          case d => <.p(css.value, d.toString)
        }.toTagMod

        <.div(
          columnData.style,
          css.column,
          <.h2(css.data, columnData.label),
          rows
        )
    }.toTagMod
  }

  def render[A <: Product]: Props[A] => VdomElement = props => {
    val columns = makeColumns(props)

    <.div(
      ^.onClick --> props.onClick,
      css.body,
      columns
    )
  }

  private def component[A <: Product] = ScalaComponent.builder[Props[A]]("Table")
    .render_P(render)
    .build

  def apply[A <: Product](headers: Vector[ColumnData], rows: Iterable[A], onClick: Callback = Callback.empty): VdomElement = {
    component[A](Props(headers, rows, onClick))
  }
}
