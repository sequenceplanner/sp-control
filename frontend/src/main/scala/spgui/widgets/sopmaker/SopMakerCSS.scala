package spgui.widgets.sopmaker

import scalacss.DevDefaults._
import spgui.theming.Theming

object SopMakerCSS extends Theming.SPStyleSheet {
  import dsl._

  val sopContainer = style(
    position.relative
  )

  val sopComponentSVG = style(
    overflow.visible.important,
  )

  val sopComponent = style(
    overflow.visible.important,
    touchAction:="none",
    userSelect := "none",
    position.absolute,
    zIndex := "1"
  )

  val opText = style(
    userSelect := "none"
  )

  val dropZone = style(
    position.absolute,
    zIndex(100),
    opacity:= "0.5"
  )
  val dropZoneOuter = style(
    position.absolute,
    zIndex(99),
    opacity:= "0.5"
  )

  val disableDropZone = style(
    pointerEvents := "none",
    visibility.hidden
  )

  val blue = style(
    backgroundColor:= "blue"
  )

  val menuOp = style(
    overflow.visible.important,
    touchAction:="none",
    userSelect := "none",
    position.relative,
    zIndex := "1",
  )

  val menuOpInner = style(
    marginBottom := -12.px
  )
  val menuOpText = style(
    fontSize := 28.px
  )

  val opStateInit = style(
    backgroundColor := "#00ff00"
  )

  val opStateExec = style(
    backgroundColor(_rgb((theme.value.spOrange)))
  )

  val opStateFini = style(
    backgroundColor := "#0000ff"
  )
  val opStateNone = style(

  )
  val opInner = style(
    height(80 px),
    width(120 px),
    borderRadius(15 px),
    borderWidth(2 px),
    borderStyle:=!"solid",
    color :=! "#000000",
    //backgroundColor := "#ffffff",
    display.flex,
    flexDirection.column
  )

  val opNameOuter = style(
    height(100 %%),
    overflow.auto,
    overflowWrap := "break-word",
    margin(4 px)
  )

  val opName = style(
    position.relative,
    textAlign.center,
    fontSize(12 px),
    overflow.hidden,
    textOverflow:= ("ellipsis"),
  )

  this.addToDocument()
}
