package spgui.widgets.model

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._


object ModelsWidgetCSS extends StyleSheet.Inline {
  import dsl._

  val selectedColor = "#4CAF50"
  val hoverColor = "#81C784"

  val container = style(
    padding := 2.em
  )

  val mainButton = style(
    margin(1.em)
  )

  val activeModelButton = style(
    outline :=! "0",
    backgroundColor :=! "transparent",
    color :=! selectedColor,
    &.hover(
      color :=! hoverColor,
      boxShadow := "none"
    ),
    &.focus(
      outline :=! "none !important",
      color :=! selectedColor,
      boxShadow := "none !important"
    ),
    &.hover.focus(
      color :=! hoverColor,
    ),

    &.active(
      outline :=! "none",
      boxShadow := "none"
    )
  )

  val inactiveModelButton = style(
    outline :=! "0",
    backgroundColor :=! "transparent",
    &.hover(
      color :=! hoverColor
    ),
    &.focus(
      outline :=! "none !important",
      boxShadow := "none !important"
    ),
    &.active(
      outline :=! "none",
      boxShadow := "none"
    )
  )

  this.addToDocument()
}
