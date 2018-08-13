package spgui

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

object CSSConfig extends StyleSheet.Inline {
  import dsl._

  val space = 15 px
  val bgColor = white
  val primaryColor = c"#F57C00"
  val secondaryColor = c"#000"
  val titleFont = "'Roboto', sans-serif"
  val tableFont = "'Lato', sans-serif"
}
