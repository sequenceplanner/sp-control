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
  val initialState = "UNKNOWN"

  val poses = List("UNKNOWN",
    "PRE_ATTACH_OF", "ATTACH_OF",

    "PRE_OF_1_UNTIGHTENED",
    "OF_1_TIGHTENED",

    "PRE_OF_2_UNTIGHTENED",
    "OF_2_TIGHTENED",
  )

  def poseMapping(source: String, target: String) = (source, target) match {
    case ("ATTACH_OF", "PRE_ATTACH_OF") => ("JOINT", "MOVEJ") // ("TCP", "MOVEL")
    case ("PRE_ATTACH_OF", "ATTACH_OF") => ("JOINT", "MOVEJ") // ("TCP", "MOVEL")
    case (_, "PRE_ATTACH_OF") => ("JOINT", "PLANNED")

    case ("PRE_OF_1_UNTIGHTENED", "OF_1_TIGHTENED") => ("JOINT", "MOVEJ") //("TCP", "MOVEL")
    case (_, "OF_1_TIGHTENED") => ("JOINT", "MOVEJ")

    case ("OF_1_TIGHTENED", "PRE_OF_1_UNTIGHTENED") => ("JOINT", "MOVEJ")
    case (_, "PRE_OF_1_UNTIGHTENED") => ("JOINT", "PLANNED")

    case ("PRE_OF_2_UNTIGHTENED", "OF_2_TIGHTENED") => ("JOINT", "MOVEJ") // ("TCP", "MOVEL")
    case (_, "OF_2_TIGHTENED") => ("JOINT", "MOVEJ")

    case ("OF_2_TIGHTENED", "PRE_OF_2_UNTIGHTENED") => ("JOINT", "MOVEJ")
    case (_, "PRE_OF_2_UNTIGHTENED") => ("JOINT", "PLANNED")

    case _ => println(s"DEFAULT CASE for $source -> $target"); ("JOINT", "PLANNED")
  }

  val needsOf = List("PRE_OF_1_UNTIGHTENED", "PRE_OF_1_TIGHTENED",
    "OF_1_UNTIGHTENED", "OF_1_TIGHTENED",

    "PRE_OF_2_UNTIGHTENED", "PRE_OF_2_TIGHTENED",
    "OF_2_UNTIGHTENED", "OF_2_TIGHTENED",
  )
  val canOnlyGoFrom = Map(
    "ATTACH_OF" -> "PRE_ATTACH_OF",
    "PRE_OF_1_UNTIGHTENED" -> "PRE_ATTACH_OF",
    "PRE_OF_2_UNTIGHTENED" -> "PRE_ATTACH_OF",
    "OF_1_TIGHTENED" -> "PRE_OF_1_UNTIGHTENED",
    "OF_2_TIGHTENED" -> "PRE_OF_2_UNTIGHTENED",
  )

}


class UR(override val system: ActorSystem) extends ROSResource {
  val actPos = i("actPos", UR.initialState, UR.poses.map(SPValue(_)))
  val prevPos = i("prevPos", UR.initialState, UR.poses.map(SPValue(_))) /// internal mirror of the last seen actual position

  val subMapping = stringToIDMapper(Map("actual_pose" -> actPos,
    "previous_pose" -> prevPos))

  val prevMapping = Flow[Map[String, SPValue]].map{ state =>
    println(state)
    state ++ state.get("actual_pose").toList.filter(_!=SPValue("UNKNOWN")).map(spval => {println("AAA: " + spval); "previous_pose" -> spval})
  }

  val subFlow = prevMapping.via(subMapping)
  subscribe("/unification_roscontrol/ur_TARS_pose_unidriver_uni_to_sp", "unification_ros2_messages/URPoseUniToSP", subFlow)

  val refPos = o("refPos", UR.initialState, UR.poses.map(SPValue(_)))
  val pubMapping = IDToStringMapper(Map(refPos -> "pose_name"))

  val attached = o("ofAttached", false)
  val attachMapping = Flow[Map[ID, SPValue]].map { state =>
    state.get(attached).map{value =>
      if(value == SPValue(true)) Map("object_action" -> SPValue("ATTACH"), "object_name" -> SPValue("OFTOOL"))
      else Map("object_action" -> SPValue("DETACH"), "object_name" -> SPValue("OFTOOL"))
    }.getOrElse(Map[String, SPValue]())
  }

  // this source can be anything! for instance configured in the frontend
  val initialProps = Map(
    "goal_tolerance" -> SPValue(0.001),
    "acc_scaling" -> SPValue(0.1),
    "speed_scaling" -> SPValue(0.1),
    "robot_name" -> SPValue("TARS"),
    "robot_type" -> SPValue("UR10")
  )
  val staticProps = Source.repeat(initialProps)

  val poseMapping = Flow[Map[ID, SPValue]].map { state =>
    val extra = (for {
      refPos <- state.get(refPos)
      actPos <- state.get(prevPos)
    } yield {
      val (poseType, actionType) = UR.poseMapping(actPos.as[String], refPos.as[String])
      Map("pose_name" -> refPos, "pose_type" -> SPValue(poseType), "action" -> SPValue(actionType))
    }).getOrElse(Map[String, SPValue]())
    extra
  }

  val pubFlow = poseMapping.zipWith(staticProps)(_++_)

  publish("/unification_roscontrol/ur_pose_unidriver_sp_to_uni", "unification_ros2_messages/URPoseSPToUni", Some(1000.millis), pubFlow)
  publish("/unification_roscontrol/scene_updater_sp_to_uni", "unification_ros2_messages/SceneUpdaterSPToUni", Some(1000.millis), attachMapping)

  a("moveToPos")(
    c("pre", "true"),
    c("started", "actPos != refPos"),
    c("post", "actPos == refPos"),
    c("reset", "true"))
}

class IPSIntegrationModel(override val system: ActorSystem) extends MiniModel {
  use("ur", new UR(system))

  val gotoPositions = UR.poses.filter(_!="UNKNOWN").map { p =>
    val of = UR.needsOf.contains(p)
    val ofc = if(of) c("pre", s"ur.ofAttached") else c("pre", "true")

    val source = UR.canOnlyGoFrom.get(p).map(source => c("pre", s"ur.actPos == '$source'")).getOrElse(c("pre", "true"))

    val op = o(s"ur.goto$p", "ur.moveToPos", "ur")(
      c("pre", s"ur.actPos != '$p' && ur.refPos != '$p'", s"ur.refPos := '$p'"),
      ofc,
      source,
      c("post", "true"),
      c("reset", "true")
    )
    (p, op)
  }.toMap

  val attach = o("ur.attach")(
    c("pre", s"!ur.ofAttached && ur.actPos == 'ATTACH_OF' && ur.refPos == 'ATTACH_OF'", "ur.ofAttached := true"),
    c("post", "true"),
    c("reset", "true")
  )

  val detach = o("ur.detach")(
    c("pre", s"ur.ofAttached && ur.actPos == 'ATTACH_OF' && ur.refPos == 'ATTACH_OF'", "ur.ofAttached := false"),
    c("post", "true"),
    c("reset", "true")
  )

  val tighten1 = SPAttributes(
    "name" -> "tighten",
    "target_state" -> "ur.actPos == _'PRE_ATTACH_OF'",
    "goal_state" -> "ur.actPos == 'OF_1_TIGHTENED'",
  )

  // main sop for testing in auto

  val mainSop = for {
    op1 <- gotoPositions.get("PRE_ATTACH_OF")
    op2 <- gotoPositions.get("ATTACH_OF")
    op3 = attach
    op4 <- gotoPositions.get("PRE_OF_1_UNTIGHTENED")
    op5 <- gotoPositions.get("OF_1_TIGHTENED")
    op6 <- gotoPositions.get("PRE_ATTACH_OF")
    op7 <- gotoPositions.get("PRE_OF_2_UNTIGHTENED")
    op8 <- gotoPositions.get("OF_2_TIGHTENED")
    op9 <- gotoPositions.get("PRE_ATTACH_OF")
    op10 <- gotoPositions.get("ATTACH_OF")
    op11 = detach
    op12 <- gotoPositions.get("PRE_ATTACH_OF")
  } yield {
    val l = List(op1, op2, op3, op4, op5, op1, op6, op7, op8, op9, op10, op11, op12).map(o=>SOP(o))
    Sequence(l)
  }

//  sopC("main sequence", mainSop.toList)

  val sopposes = Sequence(gotoPositions.map(o=>SOP(o._2)).toList)
  val sopattach = Sequence(List(SOP(attach), SOP(detach)))
  sop("UR", List(Parallel(List(sopposes, sopattach))))

  // resource bookings
  addBookings()

  exportNuXmv("ipsintegration.smv")
}
