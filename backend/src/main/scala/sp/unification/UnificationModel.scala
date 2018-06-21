package sp.unification

import sp.modelSupport._
import sp.domain.Logic._
import sp.domain._



object UnificationModel {
  val bolts = (1 to 3).map(i => s"BoltPair$i")
  def farAboveBolt(b: String) = s"FarAbove${b}TCP"
  def closeAboveBolt(b: String) = s"CloseAbove${b}TCP"
  def atBolt(b: String) = s"At${b}TCP"


  /// UR POSES
  // joint poses
  val HomeJOINT = "HomeJOINT"
  val ResetJOINT = "ResetJOINT"
  val PreAttachAtlasFarJOINT = "PreAttachAtlasFarJOINT"
  val PreAttachLFToolFarJOINT = "PreAttachLFToolFarJOINT"
  val PreAttachOFToolFarJOINT = "PreAttachOFToolFarJOINT"
  val PreFindEngineJOINT = "PreFindEngineJOINT"
  val FindEngineRightJOINT = "FindEngineRightJOINT"
  val FindEngineLeftJOINT = "FindEngineLeftJOINT"
  val FindEngineMidJOINT = "FindEngineMidJOINT"
  val AboveEngineTCP = "AboveEngineTCP"

  // above bolts
  val FarAboveBolts = bolts.map(farAboveBolt)
  val CloseAboveBolts = bolts.map(closeAboveBolt)
  val AtBolts = bolts.map(atBolt)

  // tcp poses
  val PreAttachAtlasCloseTCP = "PreAttachAtlasCloseTCP"
  val AttachAtlasTCP = "AttachAtlasTCP"
  val PreAttachLFToolCloseTCP = "PreAttachLFToolCloseTCP"
  val AttachLFToolTCP = "AttachLFToolTCP"
  val PreAttachOFToolCloseTCP = "PreAttachOFToolCloseTCP"
  val AttachOFToolTCP = "AttachOFToolTCP"
  val OFToolFrame1TCP = "OFToolFrame1TCP"
  val OFToolFrame2TCP = "OFToolFrame2TCP"
  val OFToolFrame3TCP = "OFToolFrame3TCP"
  val FindEngineRight2TCP = "FindEngineRight2TCP"
  val FindEngineLeft2TCP = "FindEngineLeft2TCP"
  val FindEngineMid2TCP = "FindEngineMid2TCP"
  val FindEngineRight3TCP = "FindEngineRight3TCP"
  val FindEngineLeft3TCP = "FindEngineLeft3TCP"
  val FindEngineMid3TCP = "FindEngineMid3TCP"

  val poses = List(
    HomeJOINT,
    ResetJOINT,
    PreAttachAtlasFarJOINT,
    PreAttachLFToolFarJOINT,
    PreAttachOFToolFarJOINT,
    PreFindEngineJOINT,
    FindEngineRightJOINT,
    FindEngineLeftJOINT,
    FindEngineMidJOINT,
    AboveEngineTCP,
    PreAttachAtlasCloseTCP,
    AttachAtlasTCP,
    PreAttachLFToolCloseTCP,
    AttachLFToolTCP,
    PreAttachOFToolCloseTCP,
    AttachOFToolTCP,
    OFToolFrame1TCP,
    OFToolFrame2TCP,
    OFToolFrame3TCP,
    FindEngineRight2TCP,
    FindEngineLeft2TCP,
    FindEngineMid2TCP,
    FindEngineRight3TCP,
    FindEngineLeft3TCP,
    FindEngineMid3TCP) ++ FarAboveBolts ++ CloseAboveBolts ++ AtBolts


  val AttachLFTool = "AttachLFTool"
  val DetachLFTool = "DetachLFTool"
  val AttachAtlas = "AttachAtlas"
  val DetachAtlas = "DetachAtlas"
  val AttachOFTool = "AttachOFTool"
  val DetachOFTool = "DetachOFTool"

  val executorCmd = List(
    AttachLFTool,
    DetachLFTool,
    AttachAtlas,
    DetachAtlas,
    AttachOFTool,
    DetachOFTool
  )

  def apply() = new UnificationModel
}

class UnificationModel extends ModelDSL {
  use("UR", new UR)
  use("Atlas", new Atlas)
  // use("MiR", new MiR)

  use("RECU", new RECU)
  use("HECU", new HECU)
  use("Executor", new Executor)


  // runner (TODO: for now runners take everything and must be on the top level of the model)




  // MAIN MODEL

  import UnificationModel._

//  // products
//  case class VnD(n: String, init: SPValue, d: List[SPValue]) {
//    def toV() = v(n, init, d)
//  }
//
//  val lf_pos = VnD("lf_pos", "on_kitting", List("on_kitting", "on_engine")).toV()


  v("lf_pos", "on_kitting", List("on_kitting", "on_engine"))
  bolts.foreach { b => v(b, "placed", List("empty", "placed", "tightened")) } // init state empty after testing
  v("filter1", "empty", List("empty", "placed", "tightened"))
  v("filter2", "empty", List("empty", "placed", "tightened"))
  v("pipes", "empty", List("empty", "placed"))

  // resources
  v("boltMode" ,"ur", List("ur", "human"))

  v("urMode", "running", List("running", "float", "stopped"))
  v("urTool", "none", List("none", "lfTool", "atlas", "filterTool"))


  val urPoseDomain = poses.map(SPValue(_)) :+ SPValue("unknown")
  println(urPoseDomain)
  val urPose = "UR.pose.ref_pos"
  v("UR.pose.ref_pos", "unknown", urPoseDomain)


  val executorDomain = executorCmd.map(SPValue(_)) :+ SPValue("reset")
  println(executorDomain)
  v("Executor.cmd", "reset", executorDomain)




  // operations


  /**
    * Initial operations
    * ******************************
    */






  /**
    * Attach LF and go to lifting pose and enter LF mode.
    * Tell op
    * ******************************
    */





  /**
    * Detach LF and attach atlas
    * Tell op to add 6 pairs of screws
    * ******************************
    */



  /**
    * Tighten the 6 nuts, and after the op, do more
    * Tell op to add the rest and pipes, and then oil filter
    *
    * ******************************
    */



  /**
    * End bolting, release atlas somewhere, change to OFTool
    * Tell op to do bolting
    *
    * ******************************
    */




  /**
    * The bolt operations
    * ******************************
    */

//  // goto above engine joint, the starting point for going to above bolt tcp poses
//  o(s"gotoAboveEngineTCP", s"UR.pose.goto_AboveEngineTCP")(
//    c("pre", s"$urPose == $HomeJOINT && urTool == 'on_engine'"),
//    c("reset", "true"))
//
//  val boltUr = c("pre", "boltMode == 'ur' && urTool == 'atlas'")
//  val boltHuman = c("pre", "boltMode == 'human' && (urTool != 'atlas' || urMode == 'float')")
//
//  // go down from far above to nutrunner position and nutrunning, then back up
//  bolts.foreach { b =>
//
//    o(s"${b}goto${closeAboveBolt(b)}", s"UR.pose.goto_${closeAboveBolt(b)}")(
//      c("pre", s"$urPose == '${farAboveBolt(b)}' && $b == 'placed'"),
//      boltUr,
//      c("reset", "true"))
//
//    o(s"${b}goto${atBolt(b)}", s"UR.pose.goto_${atBolt(b)}")(
//      c("pre", s"$urPose == '${closeAboveBolt(b)}' && $b == 'placed'"),
//      boltUr,
//      c("reset", "true"))
//
//    o(s"${b}Tighten", "Atlas.startToolForward")(
//      c("pre", s"$urPose == '${closeAboveBolt(b)}' && $b == 'placed'"),
//      boltUr,
//      c("post", "true", s"$b := 'tightened'"),
//      c("reset", "true"))
//
//    o(s"${b}backUpTo${farAboveBolt(b)}", s"UR.pose.goto_${farAboveBolt(b)}")(
//      c("pre", s"$urPose == '${atBolt(b)}' && $b == 'tightened'"),
//      boltUr,
//      c("reset", "true"))
//
//    // o(s"${b}HumanTightenMotion")(//, s"Human.tightenMotion$b")(
//    //   c("pre", s"$b == 'placed'"), boltHuman,
//    //   c("post", "true", s"urPose := 'atTCPnut$b'"),
//    //   c("reset", "true"))
//  }
//
//  // sequence, from aboveEngine to nut 1..2..3..n.. back to aboveEngine
//  val bm = bolts.zipWithIndex.map{case (b,i) => i->b}.toMap
//  bm.foreach {
//    case (0, b) => // FIRST
//      o(s"${b}goto${farAboveBolt(b)}", s"UR.pose.goto_${farAboveBolt(b)}")(
//        c("pre", s"$urPose == $AboveEngineTCP && $b == 'placed'"),
//        boltUr,
//        c("reset", "true"))
//
//    case (i, b) => // OTHERS
//      val prev = bm(i-1)
//      o(s"${b}goto${farAboveBolt(b)}", s"UR.pose.goto_${farAboveBolt(b)}")(
//        c("pre", s"$urPose == '${farAboveBolt(prev)}' && $b == 'placed' && $prev == 'tightened'"),
//        boltUr,
//        c("reset", "true"))
//  }
//
//  val lastB = bolts.last
//  o(s"${farAboveBolt(lastB)}toAboveEngine", "UR.pose.goto_AboveEngineTCP")(
//    c("pre", s"$urPose == '${farAboveBolt(lastB)}' && $lastB == 'tightened'"),
//    c("reset", "true"))
//


  /**
    * Picking and leaving the LF tool
    * ******************************
    */

  val noTool = c("pre", s"urTool == 'none'")

  o(s"DetachOFTool", s"Executor.DetachOFTool")(
    c("pre", s"$urPose == $HomeJOINT"),
    c("pre", s"urTool == 'filterTool'"),
    c("post", s"urTool == 'none'"),
    c("reset", "true"))


  o(s"gotoPreAttachLFToolFarJOINT", s"UR.pose.goto_PreAttachLFToolFarJOINT")(
    c("pre", s"$urPose == $HomeJOINT"),
    c("pre", s"lf_pos == 'on_kitting'"),
    noTool,
    c("reset", "true"))

  // goto close pre attach pose for the lf tool
  o(s"gotoPreAttachLFToolCloseTCP", s"UR.pose.goto_PreAttachLFToolCloseTCP")(
    c("pre", s"$urPose == $PreAttachAtlasFarJOINT"),
    c("pre", s"(lf_pos == 'on_kitting' && urTool == 'none') || (lf_pos == 'on_engine' && urTool == 'lfTool')"),
    c("reset", "true"))

  //unlock RrsSP connector before attaching LF tool
  o(s"releaseRspLfTool", s"RECU.unlock_rsp")(
    c("pre", s"$urPose == $PreAttachLFToolCloseTCP" ),
    noTool,
    c("reset", "true"))

  // goto attach pose for the lf tool
  o(s"gotoAttachLFToolTCP", s"UR.pose.goto_AttachLFToolTCP")(
    c("pre", s"$urPose == $PreAttachLFToolCloseTCP"),
    c("pre", s"lf_pos == 'on_kitting'"),
    noTool,
    c("reset", "true"))

  //lock RrsSP connector for the LF tool
  o(s"attachRspLfTool", s"RECU.lock_rsp")(
    c("pre", s"$urPose == $AttachLFToolTCP" ),
    noTool,
    c("post", "true", s"urTool := lfTool"),
    c("reset", "true")
  )













  runner("runner")

  // share a single driver for all ROS nodes
  // driver("ROSdriver", ROSFlatStateDriver.driverType)
}
