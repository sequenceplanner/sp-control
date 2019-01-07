package spgui.components

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

// TODO: Make CSS typesafe with Attr
object ButtonCSS extends StyleSheet.Inline {
  import dsl._

  val spButtonStyle = style("spButton")(
    backgroundColor.white,
    color :=! "#df691a",
    border :=! "2px solid #df691a",
    textAlign.center,
    textDecoration := "none",
    fontSize(16.px),
    //transitionDuration :=! "400ms",
    transitionDuration :=! inherit,
    cursor.pointer,
    padding :=! "5px 10px",
    borderRadius :=! "4px",
    marginTop :=! "0px",
    marginRight :=! "4px",
    marginBottom :=! "0px",
    whiteSpace :=! "nowrap",

    &.hover( // on hover
      backgroundColor :=! "#df691a",
      color :=! "#FFF"
    ),
    &.focus( // while focused, removes blue border
      outline :=! "0"
    ),
    &.active( // while mouse held down
      backgroundColor :=! "#d15b0c"
    )
  )

  val spDropdownButtonStyle = style("spDropdownButton")(
    backgroundColor.white,
    color :=! "#df691a",
    textAlign.center,
    textDecoration := "none",
    fontSize(16.px),
    //transitionDuration :=! "400ms",
    transitionDuration :=! inherit,
    cursor.pointer,
    position.relative,
    display.inlineBlock,
    padding :=! "5px 10px",
    margin(5.px, 10.px, 5.px, 0.px),
    whiteSpace :=! "nowrap",
    borderRadius(4.px),
    border :=! "none",

    unsafeChild("ul")(
      visibility.hidden,
      display :=! "none",
      right :=! "0",
      opacity :=! "0",
      minWidth(1.rem),
      marginTop(6.px),
      position.absolute,
/*
      unsafeChild("spButtonStyle")(
        clear.both,
        width(100.%%)
      ),*/
      &.hover(
        visibility.visible,
        opacity :=! "1",
        display :=! "block"
      )
    ),
    &.hover( // on hover
      backgroundColor :=! "#df691a",
      color :=! "#FFF",
      unsafeChild("ul")(
        visibility.visible,
        opacity :=! "1",
        display :=! "block"
      )
    ),
    &.focus( // while focused, removes blue border
      outline :=! "0"
    )
  )

  val textBeforeIcon = style(
    paddingRight :=! "6px",
  )

  val icon = style(
    transitionDuration.inherit,
    color.inherit,
    backgroundColor.inherit
  )

  val spDropdownItem = style("dd-item")(
    backgroundColor.white,
    color :=! "#df691a",
    border :=! "2px solid #df691a",
    textAlign.center,
    textDecoration := "none",
    fontSize(12.px),
    //transitionDuration :=! "400ms",
    transitionDuration :=! inherit,
    cursor.pointer,
    width :=! "100%",
    padding :=! "5px 10px",
    borderRadius :=! "4px",
    whiteSpace :=! "nowrap",

    &.hover( // on hover
      backgroundColor :=! "#df691a",
      color :=! "#FFF"
    ),
    &.focus( // while focused, removes blue border
      outline :=! "0"
    ),
    &.active( // while mouse held down
      backgroundColor :=! "#d15b0c"
    )
  )
  /*val customSPButtonCSSInSPDropdown = style(
    unsafeRoot(".spDropdownButton ul li .spButton")(
      backgroundColor.gray
    )
  )*/

  val unsortedList = style("custom_ul")(
    listStyle := "none"
  )

  val listItem = style("custom_li")(
    display.flex,
    marginBottom(1.px),
    justifyContent.flexEnd
  )

  this.addToDocument()
}

