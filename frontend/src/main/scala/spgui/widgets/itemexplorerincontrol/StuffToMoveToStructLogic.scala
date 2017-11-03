package spgui.widgets.itemexplorerincontrol

import sp.domain._


object StuffToMoveToStructLogic {

    // stolen from StructLogic
    def getChildren(node: StructNode, struct: Struct): List[StructNode] = {
      struct.items.filter(_.parent.contains(node.nodeID))
    }

    def moveNode(movedNodeID: ID, receivingNodeID: ID, struct: Struct) = {
      // TODO no validation of desired move atm
      val movedNode = struct.items.find(_.nodeID == movedNodeID).get
      // struct.items is set already in next release
      val updatedNodeSet = struct.items.toSet - movedNode + movedNode.copy(parent = Some(receivingNodeID))
      struct.copy(items = updatedNodeSet.toList)
    }

}
