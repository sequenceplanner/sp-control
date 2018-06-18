package sp.unification

import sp.modelSupport._
import sp.domain.Logic._
import sp.domain._
import sp.devicehandler._

import sp.drivers.ROSFlatStateDriver

class Turtle(name: String) extends ModelDSL {
  // turtle state
  dv("pos.x", "driver", s"turtlesim/Pose:/$name/pose:x")
  dv("pos.y", "driver", s"turtlesim/Pose:/$name/pose:y")

  // turtle commands
  dv("cmd.linear.x", "driver", s"geometry_msgs/Twist:/$name/cmd_vel:linear.x:250") // 250ms between writes
  dv("cmd.linear.y", "driver", s"geometry_msgs/Twist:/$name/cmd_vel:linear.y:250")

  // turtle abilities
  a("moveForward", List(),
    c("pre", "true", "cmd.linear.x := 5"),
    c("started", "cmd.linear.x == 5", "cmd.linear.y := -5e3"),
    c("post", "true"),
    c("reset", "true"))

  a("moveBackward", List(),
    c("pre", "true", "cmd.linear.x := -5"),
    c("started", "cmd.linear.x == -5"),
    c("post", "true", "cmd.linear.y := 0"),
    c("reset", "true"))

  // turtle operations
  o("moveForward",
    c("pre", "pos.x < 1"),
    c("post", "false"))

  o("moveBackward",
    c("pre", "pos.x > 9"),
    c("post", "false"))

  resource("resource") // blank list of things = take everything
}

class TurtleModel extends ModelDSL {
  use("turtle1", new Turtle("turtle1"))

  // v("forceX", false)
  // o("forceGoForward",
  //   c("pre", "forceX"),
  //   c("post", "false"), "turtle1.moveForward")

  // runner
  runner("turtlerunner")
  driver("driver", ROSFlatStateDriver.driverType)
}

object TurtleModel {
  def apply() = new TurtleModel
}
