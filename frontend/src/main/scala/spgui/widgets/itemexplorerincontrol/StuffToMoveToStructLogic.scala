package spgui.widgets.itemexplorerincontrol

import sp.domain._


object StuffToMoveToStructLogic {

  // stolen from StructLogic
  def getChildren(node: StructNode, struct: Struct): List[StructNode] = {
    struct.items.filter(_.parent.contains(node.nodeID)).toList
  }

  def moveNode(movedNodeID: ID, receivingNodeID: ID, struct: Struct) = {
    // TODO no validation of desired move atm
    val movedNode = struct.items.find(_.nodeID == movedNodeID).get
    val updatedNodeSet = struct.items - movedNode + movedNode.copy(parent = Some(receivingNodeID))
    struct.copy(items = updatedNodeSet)
  }

  // maybe use the function + in StructLogic
  def addItem(itemID: ID, struct: Struct) = {
    struct.copy(items = struct.items + StructNode(itemID))
  }

}
