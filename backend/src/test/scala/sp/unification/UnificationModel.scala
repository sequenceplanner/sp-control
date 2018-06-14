package sp.unification

import org.scalatest._
import sp.domain.Logic._

class VDHelperTest extends FreeSpec with Matchers {
  println
  println
  val idables = UnificationModel().buildModel("UR")
  println
  println
  println
  println
  idables.foreach { println }

}
