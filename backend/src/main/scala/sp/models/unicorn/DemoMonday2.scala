package sp.models.unicorn.demoMonday2

import scala.concurrent.duration._

import akka.stream._
import akka.stream.scaladsl._
import akka.{ NotUsed, Done }

import akka.actor._

import sp.domain.Logic._
import sp.domain._

import sp.modelSupport._
import sp.modelSupport.ros2._

object ModelData {
  val robots = List("robot1", "robot2")
  val robotInitialPoses = List("G1", "G2")

  val bins = List("GDP1F1", "GDP1F2") // , "GDP2F1", "GDP2F2")

  val robotPoses = List("unknown", "UWS", "G1", "G2", "C1", "C2", "C3", "C4", "C5", "C6") ++ bins
}


class Robot(name: String, initialPose: String, override val system: ActorSystem) extends ROSResource {
  import ModelData._

  val actPos = i("actPos", initialPose, robotPoses.map(SPValue(_)))

  val subMapping = stringToIDMapper(Map("act_pos" -> actPos))
  val subFlow = subMapping
  subscribe(s"/unicorn_roscontrol/robot$name/hrp_uni_to_sp", "unicorn_ros2_messages/HrpUniToSP", subFlow)

  val refPos = o("refPos", "unknown", robotPoses.map(SPValue(_)))
  val pubMapping = IDToStringMapper(Map(refPos -> "ref_pos"))
  val pubFlow = pubMapping
  publish(s"/unicorn_roscontrol/robot$name/hrp_sp_to_uni", "unicorn_ros2_messages/HrpSPToUni", Some(1000.millis), pubFlow)

  v("hasBin", false)

  a("planAndMove", List(refPos))(
    c("pre", "true"),
    c("isExecuting", "actPos != refPos"),
    c("post", "actPos == refPos"),
    c("reset", "true"))

}

class MondayDemo(override val system: ActorSystem) extends MiniModel {
  import ModelData._

  robots.zip(robotInitialPoses).foreach { case (r,i) =>
    use(r, new Robot(r.stripPrefix("robot"), i, system))
  }

  bins.foreach { b =>
    val byRobots = robots.map(r => s"by$r")
    // i(s"${b}", "empty", List("empty", "full"))
    i(b, "empty", (List("empty", "full") ++ byRobots).map(SPValue(_)))
    // o(s"fill$b")(
    //   c("pre", s"$b == 'empty'"),
    //   c("post", s"${b}Filled == true", s"$b := 'full'"),
    //   c("reset", "true")
    // )
  }



  // goto all poses except "unknown"
  val gotos: List[(String, String)] = List(
    // out of garage
    "G1" -> "C1",
    "G2" -> "C1",
    "C1" -> "C2") ++
    // via points going to bins
  bins.map(b => "C2" -> b) ++
    // via points returning to the uws
  bins.map(b => b -> "C6") ++
  List(
    "C2" -> "C5",
    "C6" -> "C5",
    "C5" -> "UWS",
    // back to garage
    "C5" -> "G1",
    "C5" -> "G2",
    "UWS" -> "G1",
    "UWS" -> "G2",
  )

  def gotoName(a: String, b: String): String = s"${a}to${b}"

  for {
    r <- robots
    (a,b) <- gotos
  } yield {
    val movement = gotoName(a,b)
    val otherRobots = robots.filterNot(_==r)
    val otherRobotsCannotBeAtB =
      if(otherRobots.isEmpty) "true"
      else otherRobots.map(or => s"${or}.actPos != '$b'").mkString("&&")

    val onlyGotoBinIfBinIsFull = bins.find(_==b).map(bin=>s"$b == 'full'").getOrElse("true")
    val onlyLeaveIfBinIsTaken = bins.find(_==a).map(bin=>s"$a == 'by$r'").getOrElse("true")

    val onlyGotoUWSWithBin =
      if(b == "UWS")
        bins.map(bin=>s"$bin == 'by$r'").mkString("||")
      else
        "true"

    o(s"$r.go$movement", s"$r.planAndMove", List(r,b))(
      c("pre", s"$r.actPos == '$a'", s"$r.refPos := '$b'"),
      c("pre", otherRobotsCannotBeAtB),
      c("pre", onlyGotoBinIfBinIsFull),
      c("pre", onlyLeaveIfBinIsTaken),
      c("pre", onlyGotoUWSWithBin),
      c("post", "true"),
      c("reset", "true")
    )
  }

  // pick bins
  for {
    r <- robots
    b <- bins
  } yield {
    o(s"$r.pick$b")(
      c("pre", s"$r.actPos == '$b' && $b == 'full'", s"$b := 'by$r'"),
      c("post", "true"),
      c("reset", "true")
    )
  }

  // empty bins at uws
  for {
    r <- robots
    b <- bins
  } yield {
    o(s"$r.leaveBin$b")(
      c("pre", s"$r.actPos == 'UWS' && $b == 'by$r'"),
      c("post", "true", s"$b := 'empty'"),
      c("reset", "true")
    )
  }


  // hacky sops for demo
  def s(opName: String) = SOP(operations.find(_.name == opName).get)
  robots.foreach { r =>
    val rs = SOP(List(
      Sequence(List(
        Alternative(List(
          s(s"$r.goG1toC1"),
          s(s"$r.goG2toC1"))),
        s(s"$r.goC1toC2"),
        Alternative(
          bins.map(b => Sequence(List(s(s"$r.goC2to$b"), s(s"$r.pick$b"), s(s"$r.go${b}toC6")))) ++
            List(s(s"$r.goC2toC5"))
        ),
        Alternative(List(
          Sequence(List(s(s"$r.goC5toUWS"),
            Alternative(List(s(s"$r.goUWStoG1"),s(s"$r.goUWStoG2"))))),
          s(s"$r.goC5toG1"),
          s(s"$r.goC5toG2")))))))
    sop(r, List(rs))
  }


  x("robots are never in the same spot", List("robot1.actPos == robot2.actPos"))
  x("robots never want to go to the same spot", List("robot1.refPos != 'unknown' && robot1.refPos == robot2.refPos"))


  addBookings()
  synthesize("unicorn", false)

  exportNuXmv("unicorn2.smv")

  // two robots, two bins: Nbr of states in supervisor: 148704, 69 seconds
  // two robots, four bins: supremica crashes

}
