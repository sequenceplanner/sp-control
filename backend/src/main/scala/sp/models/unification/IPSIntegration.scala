package sp.models.unification.ipsintegration

import scala.concurrent.duration._

import akka.stream._
import akka.stream.scaladsl._
import akka.{ NotUsed, Done }

import akka.actor._

import sp.domain.Logic._
import sp.domain._

import sp.modelSupport._
import sp.modelSupport.ros2._

object UR {
  val initialState = "unknown"

  val poses = List("unknown",
    "PRE_ATTACH_OF", "ATTACH_OF",

    "PRE_OF_1_UNTIGHTENED", "PRE_OF_1_TIGHTENED",
    "OF_1_UNTIGHTENED", "OF_1_TIGHTENED",

    "PRE_OF_2_UNTIGHTENED", "PRE_OF_2_TIGHTENED",
    "OF_2_UNTIGHTENED", "OF_2_TIGHTENED",
  )

  val moveTypes = Map("home" -> "joint_pose_linear_joint", "HomeUp" -> "tcp_pose_linear_joint").map { case (k,v) => SPValue(k) -> SPValue(v) }

}


class UR(override val system: ActorSystem) extends ROSResource {
  val actPos = i("actPos", UR.initialState, UR.poses.map(SPValue(_)))

  val subMapping = stringToIDMapper(Map("act_pos" -> actPos))

  val subFlow = subMapping
  subscribe("/unification_roscontrol/ur_moveit_unidriver_to_sp", "unification_ros2_messages/MoveItUniToSP", subFlow)

  val refPos = o("refPos", UR.initialState, UR.poses.map(SPValue(_)))
  val pubMapping = IDToStringMapper(Map(refPos -> "ref_pos"))

  // this source can be anything! for instance configured in the frontend
  val initialProps = Map(
    "robot_type" -> SPValue("ur10"),
    "frame" -> SPValue("local"),
    "speed_scaling" -> SPValue(0.1),
    "acc_scaling" -> SPValue(0.1),
    "should_plan" -> SPValue(false),
    "goal_tolerance" -> SPValue(0.001),
    "move_type" -> SPValue("joint_pose_linear_joint")  // todo: do in flow logic
  )

  val staticProps = Source.repeat(initialProps)
  val liveProps = fromSPMessageViaMediator("urProps").conflate((o,n)=>n).extrapolate(Iterator.continually(_), Some(Map()))
  val props = staticProps.zipWith(liveProps)(_++_)

  // flow to set the correct "move_type" -> SPValue("joint_pose_linear_joint"),
  val moveType = Flow[Map[String, SPValue]].map { case out => out }
  //   val extraField = out.get("ref_pos").flatMap{refVal => moveTypes.get(refVal).map(moveType => "move_type" -> moveType)}
  //   out ++ extraField.toList
  // }

  val pubFlow = pubMapping.via(moveType).zipWith(props)(_++_)

  publish("/unification_roscontrol/ur_moveit_sp_to_unidriver", "unification_ros2_messages/MoveItSPToUni", Some(1000.millis), pubFlow)

  a("moveToPos")(
    c("pre", "true"),
    c("started", "actPos != refPos"),
    c("post", "actPos == refPos"),
    c("reset", "true"))
}


class Demo(override val system: ActorSystem) extends MiniModel {
  use("ur", new UR(system))

  v("robot_has_tool", false)

  val urGotoAboveBlue = o("ur.gotoPreAttach", "ur.moveToPos", "ur")(
    c("pre", s"!done && ur.actPos == 'above_station' && blue == 'atTable1' && $robotDoesNotHaveTheOthersBlue", "ur.refPos := 'above_blue'"),
    c("post", "true", "ur.actPos := ur.refPos"),
    c("reset", "true")
  )

  val urOpenGripperAboveBlue = o("ur.openGripperAboveBlue", "gripper.open", "ur")(
    c("pre", s"blue == 'atTable1' && ur.actPos == 'above_blue' && gripper.actPos != 'open' && $robotDoesNotHaveTheOthersBlue"),
    c("post", "true"),
    c("reset", "true")
  )

  val urGotoAtBlue = o("ur.gotoAtBlue", "ur.moveToPos", "ur")(
    c("pre", s"blue == 'atTable1' && ur.actPos == 'above_blue' && gripper.actPos == 'open' && $robotDoesNotHaveTheOthersBlue", "ur.refPos := 'at_blue'"),
    c("post", "true"),
    c("reset", "true")
  )

  val urTakeBlue = o("ur.takeBlue", "gripper.close", "ur")(
    c("pre", s"blue == 'atTable1' && ur.actPos == 'at_blue' && gripper.actPos == 'open' && $robotDoesNotHaveTheOthersBlue"),
    c("post", "true", "blue := 'byRobot'"),
    c("reset", "true")
  )

  val urGotoAboveWithBlue = o("ur.gotoAboveWithBlue", "ur.moveToPos", "ur")(
    c("pre", s"blue == 'byRobot' && ur.actPos == 'at_blue'", "ur.refPos := 'above_station'"),
    c("post", "true"),
    c("reset", "true")
  )

  val urGotoPlaceBlue = o("ur.gotoPlaceBlue", "ur.moveToPos", "ur")(
    c("pre", "blue == 'byRobot' && ur.actPos == 'above_station'", "ur.refPos := 'place_blue'"),
    c("post", "true"),
    c("reset", "true")
  )

  val releaseBlue = o("ur.releaseBlue", "gripper.open", "ur")(
    c("pre", "blue == 'byRobot' && ur.actPos == 'place_blue'"),
    c("post", "true", "blue := 'atTable2'", "done := true"),
    c("reset", "true")
  )

  val blue = SOP(List(Sequence(List(
    SOP(urGotoAboveBlue),
    SOP(urOpenGripperAboveBlue),
    SOP(urGotoAtBlue),
    SOP(urTakeBlue),
    SOP(urGotoAboveWithBlue),
    SOP(urGotoPlaceBlue),
    SOP(releaseBlue),
  ))))

  val humanTakeRed = o("human.takeRed")(
    c("pre", "red != 'byHuman'"),
    c("post", "human.hasRed == true", "red := 'byHuman'"),
    c("reset", "true")
  )

  val humanReturnRed = o("human.returnRed")(
    c("pre", "red == 'byHuman'"),
    c("post", "human.hasRed == false", "red := 'atTable1'"),
    c("reset", "true")
  )

  val urGotoAboveRed = o("ur.gotoAboveRed", "ur.moveToPos", "ur")(
    c("pre", s"!done && ur.actPos == 'above_station' && red == 'atTable1' && $robotDoesNotHaveTheOthersRed", "ur.refPos := 'above_red'"),
    c("post", "true", "ur.actPos := ur.refPos"),
    c("reset", "true")
  )

  val urOpenGripperAboveRed = o("ur.openGripperAboveRed", "gripper.open", "ur")(
    c("pre", s"red == 'atTable1' && ur.actPos == 'above_red' && gripper.actPos != 'open' && $robotDoesNotHaveTheOthersRed"),
    c("post", "true"),
    c("reset", "true")
  )

  val urGotoAtRed = o("ur.gotoAtRed", "ur.moveToPos", "ur")(
    c("pre", s"red == 'atTable1' && ur.actPos == 'above_red' && gripper.actPos == 'open' && $robotDoesNotHaveTheOthersRed", "ur.refPos := 'at_red'"),
    c("post", "true"),
    c("reset", "true")
  )

  val urTakeRed = o("ur.takeRed", "gripper.close", "ur")(
    c("pre", s"red == 'atTable1' && ur.actPos == 'at_red' && gripper.actPos == 'open' && $robotDoesNotHaveTheOthersRed"),
    c("post", "true", "red := 'byRobot'"),
    c("reset", "true")
  )

  val urGotoAboveWithRed = o("ur.gotoAboveWithRed", "ur.moveToPos", "ur")(
    c("pre", "red == 'byRobot' && ur.actPos == 'at_red'", "ur.refPos := 'above_station'"),
    c("post", "true"),
    c("reset", "true")
  )

  val urGotoPlaceRed = o("ur.gotoPlaceRed", "ur.moveToPos", "ur")(
    c("pre", "red == 'byRobot' && ur.actPos == 'above_station'", "ur.refPos := 'place_red'"),
    c("post", "true"),
    c("reset", "true")
  )

  val releaseRed = o("ur.releaseRed", "gripper.open", "ur")(
    c("pre", "red == 'byRobot' && ur.actPos == 'place_red'"),
    c("post", "true", "red := 'atTable2'", "done := true"),
    c("reset", "true")
  )

  val red = SOP(List(Sequence(List(
    SOP(urGotoAboveRed),
    SOP(urOpenGripperAboveRed),
    SOP(urGotoAtRed),
    SOP(urTakeRed),
    SOP(urGotoAboveWithRed),
    SOP(urGotoPlaceRed),
    SOP(releaseRed),
  ))))


  val humanTakeYellow = o("human.takeYellow")(
    c("pre", "yellow != 'byHuman'"),
    c("post", "human.hasYellow == true", "yellow := 'byHuman'"),
    c("reset", "true")
  )

  val humanReturnYellow = o("human.returnYellow")(
    c("pre", "yellow == 'byHuman'"),
    c("post", "human.hasYellow == false", "yellow := 'atTable1'"),
    c("reset", "true")
  )

  val urGotoAboveYellow = o("ur.gotoAboveYellow", "ur.moveToPos", "ur")(
    c("pre", s"!done && ur.actPos == 'above_station' && yellow == 'atTable1' && $robotDoesNotHaveTheOthersYellow", "ur.refPos := 'above_yellow'"),
    c("post", "true", "ur.actPos := ur.refPos"),
    c("reset", "true")
  )

  val urOpenGripperAboveYellow = o("ur.openGripperAboveYellow", "gripper.open", "ur")(
    c("pre", s"yellow == 'atTable1' && ur.actPos == 'above_yellow' && gripper.actPos != 'open' && $robotDoesNotHaveTheOthersYellow"),
    c("post", "true"),
    c("reset", "true")
  )

  val urGotoAtYellow = o("ur.gotoAtYellow", "ur.moveToPos", "ur")(
    c("pre", s"yellow == 'atTable1' && ur.actPos == 'above_yellow' && gripper.actPos == 'open' && $robotDoesNotHaveTheOthersYellow", "ur.refPos := 'at_yellow'"),
    c("post", "true"),
    c("reset", "true")
  )

  val urTakeYellow = o("ur.takeYellow", "gripper.close", "ur")(
    c("pre", s"yellow == 'atTable1' && ur.actPos == 'at_yellow' && gripper.actPos == 'open' && $robotDoesNotHaveTheOthersYellow"),
    c("post", "true", "yellow := 'byRobot'"),
    c("reset", "true")
  )

  val urGotoAboveWithYellow = o("ur.gotoAboveWithYellow", "ur.moveToPos", "ur")(
    c("pre", "yellow == 'byRobot' && ur.actPos == 'at_yellow'", "ur.refPos := 'above_station'"),
    c("post", "true"),
    c("reset", "true")
  )

  val urGotoPlaceYellow = o("ur.gotoPlaceYellow", "ur.moveToPos", "ur")(
    c("pre", "yellow == 'byRobot' && ur.actPos == 'above_station'", "ur.refPos := 'place_yellow'"),
    c("post", "true"),
    c("reset", "true")
  )

  val releaseYellow = o("ur.releaseYellow", "gripper.open", "ur")(
    c("pre", "yellow == 'byRobot' && ur.actPos == 'place_yellow'"),
    c("post", "true", "yellow := 'atTable2'", "done := true"),
    c("reset", "true")
  )

  val yellow = SOP(List(Sequence(List(
    SOP(urGotoAboveYellow),
    SOP(urOpenGripperAboveYellow),
    SOP(urGotoAtYellow),
    SOP(urTakeYellow),
    SOP(urGotoAboveWithYellow),
    SOP(urGotoPlaceYellow),
    SOP(releaseYellow),
  ))))


  val humanTakeGreen = o("human.takeGreen")(
    c("pre", "green != 'byHuman'"),
    c("post", "human.hasGreen == true", "green := 'byHuman'"),
    c("reset", "true")
  )

  val humanReturnGreen = o("human.returnGreen")(
    c("pre", "green == 'byHuman'"),
    c("post", "human.hasGreen == false", "green := 'atTable1'"),
    c("reset", "true")
  )

  val urGotoAboveGreen = o("ur.gotoAboveGreen", "ur.moveToPos", "ur")(
    c("pre", s"!done && ur.actPos == 'above_station' && green == 'atTable1' && $robotDoesNotHaveTheOthersGreen", "ur.refPos := 'above_green'"),
    c("post", "true", "ur.actPos := ur.refPos"),
    c("reset", "true")
  )

  val urOpenGripperAboveGreen = o("ur.openGripperAboveGreen", "gripper.open", "ur")(
    c("pre", s"green == 'atTable1' && ur.actPos == 'above_green' && gripper.actPos != 'open' && $robotDoesNotHaveTheOthersGreen"),
    c("post", "true"),
    c("reset", "true")
  )

  val urGotoAtGreen = o("ur.gotoAtGreen", "ur.moveToPos", "ur")(
    c("pre", s"green == 'atTable1' && ur.actPos == 'above_green' && gripper.actPos == 'open' && $robotDoesNotHaveTheOthersGreen", "ur.refPos := 'at_green'"),
    c("post", "true"),
    c("reset", "true")
  )

  val urTakeGreen = o("ur.takeGreen", "gripper.close", "ur")(
    c("pre", s"green == 'atTable1' && ur.actPos == 'at_green' && gripper.actPos == 'open' && $robotDoesNotHaveTheOthersGreen"),
    c("post", "true", "green := 'byRobot'"),
    c("reset", "true")
  )

  val urGotoAboveWithGreen = o("ur.gotoAboveWithGreen", "ur.moveToPos", "ur")(
    c("pre", "green == 'byRobot' && ur.actPos == 'at_green'", "ur.refPos := 'above_station'"),
    c("post", "true"),
    c("reset", "true")
  )

  val urGotoPlaceGreen = o("ur.gotoPlaceGreen", "ur.moveToPos", "ur")(
    c("pre", "green == 'byRobot' && ur.actPos == 'above_station'", "ur.refPos := 'place_green'"),
    c("post", "true"),
    c("reset", "true")
  )

  val releaseGreen = o("ur.releaseGreen", "gripper.open", "ur")(
    c("pre", "green == 'byRobot' && ur.actPos == 'place_green'"),
    c("post", "true", "green := 'atTable2'", "done := true"),
    c("reset", "true")
  )

  val green = SOP(List(Sequence(List(
    SOP(urGotoAboveGreen),
    SOP(urOpenGripperAboveGreen),
    SOP(urGotoAtGreen),
    SOP(urTakeGreen),
    SOP(urGotoAboveWithGreen),
    SOP(urGotoPlaceGreen),
    SOP(releaseGreen),
  ))))

  sop("UR", List(Sequence(List(SOP(urGotoAboveStation), Parallel(List(blue,red,yellow,green))))))
  sop("Human", List(Parallel(List(Sequence(List(SOP(humanTakeBlue), SOP(humanReturnBlue))), Sequence(List(SOP(humanTakeRed), SOP(humanReturnRed))),
    Sequence(List(SOP(humanTakeYellow), SOP(humanReturnYellow))),Sequence(List(SOP(humanTakeGreen), SOP(humanReturnGreen)))))))

  // resource bookings
  addBookings()

  // x("robot cannot pick more than one brick", List("green == 'byRobot' && yellow == 'byRobot'",
  //   "blue == 'byRobot' && red == 'byRobot'",
  //   "green == 'byRobot' && red == 'byRobot'")
  // )




  // synthesis
  // reachable states: 6957024
  // time to compute: 111.195122077 seconds
  // synthesize("urdemo", true)

  // with addBookings instead of booking variables:
  // reachable states: 3895264
  // time to compute: 27.892120614 seconds

  exportNuXmv("urdemo.smv")

}
