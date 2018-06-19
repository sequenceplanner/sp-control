package sp.domain


// Move to internal relation identification. If needed, encode in attributes
case class EnabledStates(pre: Map[ID, SPValue], post : Map[ID, SPValue] = Map())
// Temporary arbiMap. Remove as soon as possible
case class EnabledStatesMap(map: Map[ID, EnabledStates], arbiMap: Set[Set[ID]] = Set())

case class RelationMap(relations: Map[Set[ID], SOP], enabledStates: EnabledStatesMap) {
  def apply(o1: ID, o2: ID) = relations(Set(o1, o2))
}


case class NoRelations(sequences: Set[List[ID]], states: Set[Map[ID, SPValue]], finalState: SPState)

