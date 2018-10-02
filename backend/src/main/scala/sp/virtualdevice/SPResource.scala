package sp.virtualdevice

import sp.domain._
import Logic._


object VirtualDeviceLogic {
  type State = Map[ID, SPValue]

  case class ResourcePipeline(message: Struct, flowDef: SPAttributes)
  case class SPResource(resource: ID,
                        initialState: State,
                        outgoing: List[ResourcePipeline],
                        incoming: List[ResourcePipeline]
                        )
  case class SPVDRunner(ops: List[Operation],
                        initialState: State,
                        stateVariables: Struct,
                        transitionSystem: List[sp.runners.RunnerLogic.OperationTransition]
                       )

  case class SPVD(vd: ID,
                  items: List[IDAble],
                  resources: List[SPResource],
                  runner: SPVDRunner
                 )


  // move somewhere
  /**
    * Is checking that all variables have an initial state.
    * @param s
    * @param structs
    * @param ops
    * @return
    */
  def validateInitialState(s: State, structs: List[Struct], ops: List[Operation]) = {
    def extractLeafs(tree: Struct) = tree.getChildrenMap.collect{case (id, ch) if ch.isEmpty => id}
    val ids = structs.flatMap(extractLeafs) ++ ops.map(_.id)
    ids.forall(s.contains)
  }



}



