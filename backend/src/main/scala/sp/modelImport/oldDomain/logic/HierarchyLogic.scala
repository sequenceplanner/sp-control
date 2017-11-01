package sp.modelImport.oldDomain.logic

import sp.modelImport.oldDomain._

object HierarchyLogic extends HierarchyLogics

trait HierarchyLogics {
  implicit class HierarchyExtras(x: HierarchyRoot){

    def getAllIDs: List[ID] = {
      getAllNodes.map(_.item)
    }

    def getChildren(id: ID): List[ID] = {
      getAllNodes.filter(_.item == id).flatMap(_.children.map(_.item))
    }

    def getParent(id: ID): Option[ID] = {
      getAllNodes.find(_.children.exists(_.item == id)).map(_.item)
    }

    def getNodes(f: HierarchyNode => Boolean) = {
      getAllNodes.filter(f)
    }

    def getAllNodes: List[HierarchyNode] = {
      def itr(node: HierarchyNode): List[HierarchyNode] = {
        node :: node.children.flatMap(itr)
      }
      x.children.flatMap(itr)
    }
  }

}
