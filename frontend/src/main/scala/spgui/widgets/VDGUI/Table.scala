package spgui.widgets.VDGUI

import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.component.Scala
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._


object Table {
  import spgui.widgets.VDGUI.CSSHelpers.toHtml
  import spgui.widgets.VDGUI.{TableCSS => css}

  case class Props[A <: Product](header: A, rows: Iterable[A], onClick: Callback)

  def makeColumns[A <: Product](props: Props[A]): TagMod = {
    val columnData = (props.header :: props.rows.toList).foldLeft(Map[Int, Vector[Any]]()) { (acc, next) =>
      next.productIterator.zipWithIndex.foldLeft(acc) { case (acc2, (v, i)) =>
        val curr = acc2.getOrElse(i, Vector())
        acc2.updated(i, curr :+ v)
      }
    }

    columnData.values.filter(_.nonEmpty).map { data =>
      val header = data(0)
      val rows = data.tail.map(d => <.p(css.value, d.toString)).toTagMod
      <.div(
        ^.onClick --> (Callback(println("Click.")) >> props.onClick),
        css.column,
        <.h2(css.data, header.toString),
        rows
      )
    }.toTagMod
  }

  def render[A <: Product]: Props[A] => VdomElement = props => {
    val columns = makeColumns(props)

    <.div(
      ^.onClick --> (Callback(println("Click.")) >> props.onClick),
      css.body,
      columns
    )
  }

  private def component[A <: Product] = ScalaComponent.builder[Props[A]]("Table")
    .render_P(render)
    .build

  type UnmountedTable[A <: Product] = Scala.Unmounted[Props[A], Unit, Unit]
  def apply[A <: Product](header: A, rows: Iterable[A], onClick: Callback = Callback.empty): UnmountedTable[A] = component[A](Props(header, rows, onClick))
}
