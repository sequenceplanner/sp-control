//package sp.virtualdevice
//
//  import sp.domain._
//  import Logic._
//
//
//object VirtualDevice{
//  type State = Map[ID, SPValue]
//}
//import VirtualDevice._
//
//case class StateDefinition(outputDefinition: Struct,
//                           inputDefinition: Struct,
//                           items: Map[ID, IDAble]
//                          )
//
//
//trait SPStreams {
//  val definition: StateDefinition
//}
//trait SPStreamSink extends SPStreams{
//  def StateHasChanged(state: State): Unit // or a future for OK or Error. Or a source for getting multiple messages about what is going on
//}
//trait SPStreamSource extends SPStreams{
//   // will be called when new variables arrive
//  def registerCallback(f: State => Unit)
//}
//
//
//
//
//
//case class SPResource(resource: Thing,
//                      stateDefinition: StateDefinition,
//                      initialState: State,
//                      outputStreams: List[SPStreamSink],
//                      inputStreams: List[SPStreamSource]
//                     ) {
//
//
//  def getInitialState: State = {
//    def extractLeafs(tree: Struct) = tree.getChildrenMap.collect{case (id, ch) if ch.isEmpty => id}
//    val outStateIds = outputStreams.flatMap(x => extractLeafs(x.definition.outputDefinition) ++ extractLeafs(x.definition.inputDefinition))
//    val inStateIds = inputStreams.flatMap(x => extractLeafs(x.definition.outputDefinition) ++ extractLeafs(x.definition.inputDefinition))
//
//    (outStateIds ++ inStateIds).map(id => id -> initialState.getOrElse(id, SPValue(""))).toMap
//  }
//}
//
//case class SPAbility(ability: Ability,
//                     preCondition: Condition = Condition(AlwaysFalse, List()),
//                     started: Condition = Condition(AlwaysTrue, List()),
//                     postCondition: Condition = Condition(AlwaysTrue, List()),
//                     resetCondition: Condition = Condition(AlwaysTrue, List()),
//                     parameters: List[Thing],
//                     results: List[Thing]
//                    ) {
//
//}
//
//
//trait SPVirtualDevice {
//  val vd: Thing
//  val resources: List[SPResource]
//  val publishState: SPState => Unit
//
//  private var state: State = resources.flatMap(_.getInitialState).toMap
//
//  def triggerStreams(s: State) = {
//    resources.foreach(_.outputStreams.foreach(_.StateHasChanged(s)))
//  }
//  def sendOutState(s: State) = {
//    publishState(SPState(vd.name + "_state", s, SPAttributes("vd"->vd.id))) // add attributes and things here. New ID per state change
//  }
//
//
//  def updState(s: State) = {
//    val old = state
//    state = state ++ s
//    if (old != state) {
//      triggerStreams(state)
//      publishState(state)
//    }
//  }
//
//
//}
