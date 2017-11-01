package spgui.widgets.itemeditorincontrol

import scalajs.js
import js.Dynamic.{ literal => l }
import scala.util.Try

object ItemEditorOptions {
  def apply() =
    JSONEditorOptions(
      mode = "code",
      schema = itemEditorSchema,
      onEditable = onItemNodeEditable
    )

  def onItemNodeEditable(node: js.Dynamic): js.Dynamic = {
    // node is part of jsonEditor API and has members field, value and path
    if(js.isUndefined(node) || js.isUndefined(node.selectDynamic("field"))) l("field" -> true, "value" -> true)
    else {
      // field names never editable, values editable except the one for id
      val fieldNameEditable = false
      val fieldisID = Try(node.selectDynamic("field").asInstanceOf[String]).map(_=="id").getOrElse(false)
      val valueEditable = !fieldisID
      l("field" -> fieldNameEditable, "value" -> valueEditable)
    }
  }

  val itemEditorSchema = l(
    "title" -> "SP item",
    "type" -> "object",
    "properties" -> l(
      "isa" -> l(
        "enum" -> js.Array("HierarchyRoot", "Operation", "SOPSpec")
      ),
      "name" -> l(
        "type" -> "string"
      ),
      /*
       "age" -> l(
       "description" -> "Age in years",
       "type" -> "integer",
       "minimum" -> 0
       ),
       */
      "id" -> l(
        // dk if this is visible somewhere, but I think it would be nice
        "description" -> "UUID as string",
        "type" -> "string"
        // // jsoneditor doesn't care, it seems, should facade onEditable
        // "readOnly" -> true
      )
    ),
    "required" -> js.Array("isa", "name", "id")
  )

}
