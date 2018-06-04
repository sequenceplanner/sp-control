package sp.unification

import sp.domain.Logic._
import sp.domain._
import sp.devicehandler._

import sp.drivers.ROSFlatStateDriver

class TurtleModel(n: String) extends VDHelper {
  val name = n

  // turtle state
  dv("pos.x", "driver", s"turtlesim/Pose:/$name/pose:x")
  dv("pos.y", "driver", s"turtlesim/Pose:/$name/pose:y")

  // turtle commands
  dv("cmd.linear.x", "driver", s"geometry_msgs/Twist:/$name/cmd_vel:linear.x:250") // 250ms between writes
  dv("cmd.linear.y", "driver", s"geometry_msgs/Twist:/$name/cmd_vel:linear.y:250")

  // turtle abilities
  a("moveForward", List(),
    ac("pre", "true", "cmd.linear.x := 5"),
    ac("started", "cmd.linear.x == 5", "cmd.linear.y := -5e3"),
    ac("post", "true"),
    ac("reset", "true"))

  a("moveBackward", List(),
    ac("pre", "true", "cmd.linear.x := -5"),
    ac("started", "cmd.linear.x == -5"),
    ac("post", "true", "cmd.linear.y := 0"),
    ac("reset", "true"))

  // turtle operations
  o("moveForward",
    oc("pre", "pos.x < 1"),
    oc("post", "false"))

  o("moveBackward",
    oc("pre", "pos.x > 9"),
    oc("post", "false"))

  // runner
  r("turtlerunner", initState = Map())

  // drivers and resources
  driver("driver", ROSFlatStateDriver.driverType)
  resource("resource") // blank list of things = take everything
}

object TurtleModel {
  def apply(name: String = "turtle1") = new TurtleModel(name)
}
