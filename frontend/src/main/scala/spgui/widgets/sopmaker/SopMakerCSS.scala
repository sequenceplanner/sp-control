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
    zIndex(1)
  )

  val opText = style(
    userSelect := "none"
  )

  val dropZone = style(
    position.absolute,
    zIndex(100),
    opacity(0.5)
  )
  val dropZoneOuter = style(
    position.absolute,
    zIndex(99),
    opacity(0.5)
  )

  val disableDropZone = style(
    pointerEvents := "none",
    visibility.hidden
  )

  val blue = style(
    backgroundColor.blue
  )

  val menuOp = style(
    overflow.visible.important,
    touchAction:="none",
    userSelect := "none",
    position.relative,
    zIndex(1)
  )

  val menuOpInner = style(
    marginBottom(-12 px)
  )
  val menuOpText = style(
    fontSize(28 px)
  )

  // The following flickers too much, use less color
  // val opStateNotEnabled = style(
  //   backgroundColor.rgb(240, 128, 128)
  // )

  // val opStateEnabled = style(
  //   backgroundColor.rgb(255, 255, 255)
  // )

  // val opStateStarting = style(
  //   backgroundColor.rgb(255, 228, 181)
  // )

  // val opStateExecuting = style(
  //   backgroundColor.rgb(144, 238, 144)
  // )

  // val opStateFinished = style(
  //   backgroundColor.rgb(100, 149, 237)
  // )


  val opStateNotEnabled = style(
    backgroundColor.rgb(255, 255, 255)
  )

  val opStateEnabled = style(
    backgroundColor.rgb(200, 200, 255)
  )

  val opStateStarting = style(
    backgroundColor.rgb(144, 238, 144)
  )

  val opStateExecuting = style(
    backgroundColor.rgb(144, 238, 144)
  )

  val opStateFinished = style(
    backgroundColor.rgb(255, 255, 255)
  )

  val opStateNone = style(

  )
  val opInner = style(
    height(80 px),
    width(200 px),
    borderRadius(5 px),
    borderWidth(2 px),
    borderStyle.solid,
    color.rgb(0,0,0),
    backgroundColor.rgb(255, 255, 255),
    display.flex,
    flexDirection.column
  )

  // used by OperationRunnerCardComponent
  val tinyOpInner = style(
    height(50 px),
    width(80 px),
    borderRadius(5 px),
    borderWidth(2 px),
    borderStyle.solid,
    color.rgb(0, 0, 0),
    backgroundColor.rgb(255, 255, 255),
    display.flex,
    flexDirection.column
  )

  val opNameOuter = style(
    height(100 %%),
    overflow.auto,
    overflowWrap := "break-word"
  )

  val opName = style(
    position.relative,
    textAlign.center,
    fontSize(12 px),
    overflow.hidden,
    textOverflow:= ("ellipsis"),
    fontFamily :=! "monospace"
  )

  val preCondition = style(
    width(100 %%),
    alignSelf.center,
    height(20 px),
    borderBottom :=! "solid 1px"
  )

  val postCondition = style(
    width(100 %%),
    alignSelf.center,
    height(20 px),
    borderTop :=! "solid 1px"
  )

  this.addToDocument()
}
