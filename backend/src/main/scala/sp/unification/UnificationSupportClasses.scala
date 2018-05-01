package sp.unification

import sp.domain.{ID, Operation, SPValue, Thing}

// Some support classes..
case class RobotOperations(ops: List[Operation],
                           vars: List[Thing],
                           init: Map[ID, SPValue],
                           refID: ID,
                           current: ID,
                           activateID: ID
                          )
