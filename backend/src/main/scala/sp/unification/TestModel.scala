package sp.unification

import sp.modelSupport._
import sp.devicehandler._
import sp.domain.Logic._
// import sp.drivers.{ROSFlatStateDriver, URDriver}

class TestDummy extends ModelDSL {
  v("x", "a", List("a", "b", "c"))
  v("active", "no", List("no", "yes"))


  o("xo")(
    c("pre", "active == 'yes' && x == a", "x := b"),
    c("post", "x == b", "x := c"),
  )

  o("xo2")(
    c("pre", "active == 'yes' && x == c", "x := b"),
    c("post", "x == b", "x := a")
  )

  runner("runner")
  resource("resource") // blank list of things = take everything
}

object TestDummy {
  def apply() = new TestDummy
}
