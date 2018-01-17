package sp.virtcom

import sp.domain._
import sp.domain.Logic._
import sp.patrikmodel.CollectorModel


object GetCases extends GetCas
trait GetCas {

  def cases(SopID : ID, ids : List[IDAble]): Map[String, List[Operation]] ={

    val sopSpec = ids.find(idable => idable.id == SopID).get.asInstanceOf[SOPSpec]
    val ops = ids.filter(_.isInstanceOf[Operation]).map(_.asInstanceOf[Operation]) // filter out the operations from the ids


    var caseMap = Map[String,List[Operation]]()
    sopSpec.sop.foreach(individualSop => {
      getCasesFromSop(individualSop, false)
    })

    def getCasesFromSop(individualSop :SOP, alt :Boolean) : List[Operation] ={ // Gets the cases as operations out of the SOP
      var caseList = List[Operation]()
      var alternative = alt

      if(individualSop.isInstanceOf[Sequence]) {
        individualSop.sop.foreach(sopNode => {
          caseList ++= getCasesFromSop(sopNode, alternative)// call the function again with the sopNode Sop
          alternative = false // I only want the first operation of the case sequence.
        })
      }

      else if(individualSop.isInstanceOf[OperationNode]){
        var op = ops.filter(op => op.id == individualSop.asInstanceOf[OperationNode].operation).head // get the ID from the node, find the corresponding operation
        if(alternative){caseList :+= op}
      }

      else if(individualSop.isInstanceOf[Alternative]){
        individualSop.sop.foreach(sopNode => {
          caseList ++= getCasesFromSop(sopNode, true) // call the function again with the sopNode Sop
        })
        if(caseList.nonEmpty) {
          val robotSchedule = caseList.head.attributes.getAs[String]("robotSchedule").getOrElse("error") // get robot schedule name.
          val caseName = caseList.head.name.substring(robotSchedule.length + 1, caseList.head.name.length).replaceAll("[^a-zA-Z]+", "")
          caseMap += ((robotSchedule + "_" + caseName) -> caseList)
        }
        caseList = List[Operation]()
      }
      caseList
    }
    caseMap
  }
}
