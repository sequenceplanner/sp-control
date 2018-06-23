package sp.unification

import sp.domain.SPAttributes
import sp.modelSupport._
import sp.domain._
import sp.domain.Logic._


class TON(name: String) extends ModelDSL {
  val dn=name + "driver"
  val rn= name + "resource"

  dv("in", dn,"in")
  dv("pt", dn,"pt")
  dv("q", dn,"q")

  v("pt", 1000, List(1000, 2000, 3000))

  a("delay", List("pt"),
    c("pre", "NOT in", s"in := true"),
    c("started", s"in"),
    c("post", "q || NOT in", "in := false"),
    c("reset", "true", "in := false")
  )

  driver(dn, sp.drivers.TONDriver.driverType)
  resource(rn)
}
