package sp.unification

import sp.domain.SPAttributes
import sp.drivers.ros2._
import sp.modelSupport._


class Operator(name: String) extends ModelDSL {
  import sp.unification.UnificationModel._

  val dn=name + "driver"
  val rn= name + "resource"

  dv("humanName", dn,"humanName")
  dv("humanID", dn,"humanID")
  dv("loggedIn", dn,"loggedIn")
  dv("cmd", dn,"cmd")
  dv("ack", dn,"ack")
  dv("done", dn,"done")
  dv("alert", dn,"alert")


  instructions.keys.foreach { an =>
    a(an, List(),
      c("pre", "true", s"cmd := $an"),
      c("started", "ack || done"),  // maybe change to cmd != ''
      c("post", "done", "cmd := ''", "ack := false", "done := false"),
      c("reset", "true")
    )
  }

  a("alert", List(),
    c("pre", "true", s"alert := 'ALERT'"),
    c("started", s"alert != ''"),
    c("post", "alert == ''"),
    c("reset", "true")
  )

  driver(dn, sp.drivers.HumanDriver.driverType, SPAttributes("instructions" -> instructions))
  resource(rn) // blank list of things = take everything
}

class ReadLogIn extends ModelDSL with ROS2ModelSupport {
  import sp.unification.UnificationModel._

  reader("OperatorLogIN", "std_msgs/Bool", "/rfid/login_authentication")

  driver("OperatorLogIN", ROS2FlatStateDriver.driverType)
  // blank list of things = take everything
  resource("resource")
}
