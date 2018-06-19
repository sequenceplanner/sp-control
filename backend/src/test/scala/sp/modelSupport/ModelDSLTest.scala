package sp.modelSupport

import org.scalatest._
import sp.domain.Logic._
import sp.domain._
import sp.drivers.ROSFlatStateDriver

class Turtle(name: String) extends ModelDSL {
  // turtle state
  dv("pos.x", "rosDriver", s"turtlesim/Pose:/$name/pose:x")
  dv("pos.y", "rosDriver", s"turtlesim/Pose:/$name/pose:y")

  // turtle commands
  dv("cmd.linear.x", "rosDriver", s"geometry_msgs/Twist:/$name/cmd_vel:linear.x:250") // 250ms between writes
  dv("cmd.linear.y", "rosDriver", s"geometry_msgs/Twist:/$name/cmd_vel:linear.y:250")

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
  o("moveForward")(
    c("pre", "pos.x < 1"),
    c("post", "false"))

  o("moveBackward")(
    c("pre", "pos.x > 9"),
    c("post", "false"))


  resource(s"resource", List("cmd.linear.x")) // blank list of things = take everything
}

class TurtleModel(name: String) extends ModelDSL {
  use("turtle1", new Turtle("turtle1"))

  // v("forceX", false)
  // o("forceGoForward",
  //   c("pre", "forceX"),
  //   c("post", "false"), "turtle1.moveForward")

  // runner
  runner("turtlerunner", initState = Map("forceX" -> true))

  // drivers and resources
  driver("rosDriver", ROSFlatStateDriver.driverType)
}


class VDHelperTest extends FreeSpec with Matchers {

  val s = VariableKind.fromString("ReadOnly")
  assert(s == Some(ReadOnly))


  println
  println



  val t = new TurtleModel("hej2")

  t.mes.foreach { println }

  println
  println

  val idables = t.build("test", t.mes)
  println
  println
  println
  println
  idables.foreach { println }



  println
  println
}
