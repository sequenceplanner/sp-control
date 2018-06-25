package sp.unification

import sp.modelSupport._
import sp.domain.Logic._
import sp.domain._



object UnificationModel {
  val bolts = (1 to 12).map(i => s"BoltPair$i")
  def farAboveBolt(b: String) = s"FarAbove${b}TCP"
  def closeAboveBolt(b: String) = s"CloseAbove${b}TCP"
  def atBolt(b: String) = s"At${b}TCP"


  /// UR POSES
  // joint poses
  val HomeJOINT = "HomeJOINT"
  val PreAttachAtlasFarJOINT = "PreAttachAtlasFarJOINT"
  val PreAttachLFToolFarJOINT = "PreAttachLFToolFarJOINT"
  val PreAttachOFToolFarJOINT = "PreAttachOFToolFarJOINT"
  val PreFindEngineJOINT = "PreFindEngineJOINT"
  val FindEngineRightUpJOINT = "FindEngineRightUpJOINT"
  val FindEngineLeftUpJOINT = "FindEngineLeftUpJOINT"
  val FindEngineMidUpJOINT = "FindEngineMidUpJOINT"
  val AboveEngineTCP = "AboveEngineTCP"
  val LFOperationMidpoint1JOINT = "LFOperationMidpoint1JOINT"
  val LFOperationMidpoint2JOINT = "LFOperationMidpoint2JOINT"
  val LFOperationMidpoint3JOINT = "LFOperationMidpoint3JOINT"
  val LFOperationMidpoint4JOINT = "LFOperationMidpoint4JOINT"
  val LFOperationMidpoint5JOINT = "LFOperationMidpoint5JOINT"
  val AfterLFOperationJOINT = "AfterLFOperationJOINT"
  val PreHomeJOINT = "PreHomeJOINT"
  val OFMidpoint1JOINT = "OFMidpoint1JOINT"
  val OFMidpoint2JOINT = "OFMidpoint2JOINT"

  // above bolts
  val FarAboveBolts = bolts.map(farAboveBolt)
  val CloseAboveBolts = bolts.map(closeAboveBolt)
  val AtBolts = bolts.map(atBolt)

  // tcp poses
  val PreAttachAtlasCloseTCP = "PreAttachAtlasCloseTCP"
  val AttachAtlasTCP = "AttachAtlasTCP"
  val PreAttachLFToolCloseTCP = "PreAttachLFToolCloseTCP"
  val AAPRAtlasTCP = "AAPRAtlasTCP"
  val AttachLFToolTCP = "AttachLFToolTCP"
  val PreAttachOFToolCloseTCP = "PreAttachOFToolCloseTCP"
  val AttachOFToolTCP = "AttachOFToolTCP"
  val OFToolFrame1TCP = "OFToolFrame1TCP"
  val OFToolFrame2TCP = "OFToolFrame2TCP"
  val OFToolFrame3TCP = "OFToolFrame3TCP"
  val FindEngineRightDownTCP = "FindEngineRightDownTCP"
  val FindEngineLeftDownTCP = "FindEngineLeftDownTCP"
  val FindEngineMidDownTCP = "FindEngineMidDownTCP"
  val FindEngineRightCollideTCP = "FindEngineRightCollideTCP"
  val FindEngineLeftCollideTCP = "FindEngineLeftCollideTCP"
  val FindEngineMidCollideTCP = "FindEngineMidCollideTCP"

  val poses = List(
    HomeJOINT,
    PreAttachAtlasFarJOINT,
    PreAttachLFToolFarJOINT,
    PreAttachOFToolFarJOINT,
    PreFindEngineJOINT,
    AAPRAtlasTCP,
    FindEngineRightUpJOINT,
    FindEngineLeftUpJOINT,
    FindEngineMidUpJOINT,
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
    FindEngineRightDownTCP,
    FindEngineLeftDownTCP,
    FindEngineMidDownTCP,
    FindEngineRightCollideTCP,
    FindEngineLeftCollideTCP,
    FindEngineMidCollideTCP,
    LFOperationMidpoint1JOINT,
    LFOperationMidpoint2JOINT,
    LFOperationMidpoint3JOINT,
    LFOperationMidpoint4JOINT,
    LFOperationMidpoint5JOINT,
    PreHomeJOINT,
    AfterLFOperationJOINT,
    OFMidpoint1JOINT,
    OFMidpoint2JOINT
  ) ++ FarAboveBolts ++ CloseAboveBolts ++ AtBolts


  val AttachLFTool = "AttachLFTool"
  val DetachLFTool = "DetachLFTool"
  val AttachAtlas = "AttachAtlas"
  val DetachAtlas = "DetachAtlas"
  val AttachOFTool = "AttachOFTool"
  val DetachOFTool = "DetachOFTool"
  val LFMagic = "LFMagic"

  val executorCmd = List(
    AttachLFTool,
    DetachLFTool,
    AttachAtlas,
    DetachAtlas,
    AttachOFTool,
    DetachOFTool,
    LFMagic
  )


  //The instructions to the operator
  val instructions = Map(
    "login" -> "You need to log in before working",
    "mountLF" -> "Mount the LF on the engine. The robot will help",
    "placeB1to6" -> "Place the first 6 pairs of screws",
    "placeBRest" -> "Place rest of screws",
    "placePipes" -> "Place the pipes",
    "placeOF" -> "Place the oil filter",
    "atlasOF" -> "Use the nut-runner to tighten the bolts",
  )




  def apply() = new UnificationModel
}

class UnificationModel extends ModelDSL {
  use("UR", new UR)
  use("Atlas", new Atlas)
  use("MiR", new MiR)
  use("RECU", new RECU)
  use("HECU", new HECU)
  use("Executor", new Executor)
  use("OP", new Operator("OP"))
  use("TON", new TON("TON"))

  // booking by not allowing starting if another operation is in executing
  val useUR = List("UR")
  val useRSP = List("RSP")
  val useOP = List("OP")
  val useTON = List("TON")


  // helper. To make a v for boolean variables
  def bv = (name: String) => v(name, false, List(true, false))
  // runner (TODO: for now runners take everything and must be on the top level of the model)




  // MAIN MODEL

  import UnificationModel._

  v("lf_pos", "on_kitting", List("on_kitting", "on_engine"))
  bolts.foreach { b => v(b, "placed", List("empty", "placed", "tightened")) } // init state empty after testing
  v("filter1", "empty", List("empty", "placed", "tightened"))
  v("pipes", "empty", List("empty", "placed"))
  bv("allBolts")

  // resources
  v("boltMode" ,"ur", List("ur", "human"))
  v("urMode", "running", List("running", "float", "stopped"))
  v("urTool", "none", List("none", "lfTool", "atlas", "ofTool"))
  v("mir", "outside", List("outside", "atEngine"))
  v("engine", "notMeasured", List("outside", "notMeasured", "measured"))


  bv("OP.loggedIn")
  v("OP.humanName", "", List("", "Karen", "StandIN"))
  v("OP.humanID", "", List("", "1234"))
  v("TON.pt", 1000, List(1000, 2000, 3000))



  val urPoseDomain = poses.map(SPValue(_)) :+ SPValue("unknown")
  //println(urPoseDomain)
  val urPose = "UR.pose.act_pos"
  v("UR.pose.act_pos", "unknown", urPoseDomain)




  // operations


  /**
    * Initial operations
    * ******************************
    */

  o("log_in", s"OP.login", useOP)(
    c("pre", "NOT OP.loggedIn"),
    c("reset", "true")
  )
  // add another operation that is forwarding the log in

  o("mir_enter", "MiR.pose.goto_Assembly")( // add mir ability to move in
    c("pre", "mir == 'outside' && OP.loggedIn"),
    c("post", "true", "mir := 'atEngine'"),
    c("reset", "true")
  )

// For all three sides...
//  sop("measureEngine")(
//    c("pre", s"OP.loggedIn && engine == 'notMeasured' && $urPose == $HomeJOINT"),
//    c("pre", s"urTool == 'none'")
//  )(List(
//      sOnew("toInitPosMeasureEngine", s"UR.pose.goto_PreFindEngineJOINT", useUR)()
//    ) ++ List("Right", "Left", "Mid").flatMap{ x =>
//        List(
//          sOnew(s"findingEngine${x}UpPre", s"UR.pose.goto_FindEngine${x}UpJOINT", useUR)(),
//          sOnew(s"findingEngine${x}Down", s"UR.pose.goto_FindEngine${x}DownTCP", useUR)(),
//          //sOnew(s"delayBefore${x}Collide", s"TON.delay", useTON)(cond("pre", "true", "TON.pt := 1000")),
//          sOnew(s"findingEngine${x}Collide", s"UR.pose.goto_FindEngine${x}CollideTCP", useUR)(),
//          sOnew(s"AfterCollideEngine${x}UpPre", s"UR.pose.goto_FindEngine${x}UpJOINT", useUR)()
//        )
//      } ++ List(
//      sOnew("toAfterPosMeasureEngine", s"UR.pose.goto_PreFindEngineJOINT", useUR)(
//        cond("pre", s"true", "engine := 'measured'")
//      ),
//      sOnew("toHomeAfterMeasure", s"UR.pose.goto_HomeJOINT", useUR)(),
//    ):_*
//  )


  // just one side for demo
//  sop("measureEngineSimple")(
//    c("pre", s"OP.loggedIn && engine == 'notMeasured' && $urPose == $HomeJOINT"),
//    c("pre", s"urTool == 'none'"),
//    c("reset", "true")
//  )(List("Right").flatMap{ x =>
//    List(
//      sOnew("toInitPosMeasureEngine", s"UR.pose.goto_PreFindEngineJOINT", useUR)(),
//      sOnew(s"findingEngine${x}UpPre", s"UR.pose.goto_FindEngine${x}UpJOINT", useUR)(),
//      sOnew(s"findingEngine${x}Down", s"UR.pose.goto_FindEngine${x}DownTCP", useUR)(),
//      //sOnew(s"delayBefore${x}Collide", s"TON.delay", useTON)(cond("pre", "true", "TON.pt := 1000")),
//      sOnew(s"findingEngine${x}Collide", s"UR.pose.goto_FindEngine${x}CollideTCP", useUR)(),
//      sOnew(s"AfterCollideEngine${x}UpPre", s"UR.pose.goto_FindEngine${x}UpJOINT", useUR)()
//    )
//  } ++ List(
//    sOnew("toAfterPosMeasureEngine", s"UR.pose.goto_PreFindEngineJOINT", useUR)(
//      cond("pre", s"true", "engine := 'measured'")
//    ),
//    sOnew("toHomeAfterMeasure", s"UR.pose.goto_HomeJOINT", useUR)(),
//  ):_*
//  )





  /**
    * Attach LF and go to lifting pose and enter LF mode.
    * Tell op
    * ******************************
    */

  sop("attachLFToolWithExecutor")(
    c("pre", s"OP.loggedIn && engine == 'measured' && $urPose == $HomeJOINT"),
    c("pre", s"urTool == 'none'"),
    c("pre", s"lf_pos == 'on_kitting'"),
    c("reset", "true")
  )(
    sOnew("toPreAttachLF", s"UR.pose.goto_PreAttachLFToolFarJOINT", useUR)(),
    sOnew(s"AttachLFAfterMeasure", s"Executor.$AttachLFTool", useUR)(
      c("post", "true", s"urTool := 'lfTool'")
    ),
    sOnew("toHomeAfterLF", s"UR.pose.goto_HomeJOINT", useUR)(),
  )

  sop("goToMirToDoLF")(
    c("pre", s"mir == 'atEngine' && $urPose == $HomeJOINT"),
    c("pre", s"$urPose == $HomeJOINT"),
    c("pre", s"urTool == 'lfTool'"),
    c("pre", s"lf_pos == 'on_kitting'"),
    c("reset", "true")
  )(
    sOnew("toLFMagic1", s"UR.pose.goto_$LFOperationMidpoint1JOINT", useUR)(),
    sOnew("toLFMagic2", s"UR.pose.goto_$LFOperationMidpoint2JOINT", useUR)(),
    sOnew("toLFMagic3", s"UR.pose.goto_$LFOperationMidpoint3JOINT", useUR)(),
    sOnew("toLFMagic4", s"UR.pose.goto_$LFOperationMidpoint4JOINT", useUR)(),
    sOnew("toLFMagicFinal", s"UR.pose.goto_$LFOperationMidpoint5JOINT", useUR)(),
  )



  sop("doLFMagicAndBack")(
    c("pre", s"mir == 'atEngine'"),
    c("pre", s"$urPose == $LFOperationMidpoint5JOINT"),
    c("pre", s"urTool == 'lfTool'"),
    c("pre", s"lf_pos == 'on_kitting'"),
    c("reset", "true")
  )(
    sP(
      sOnew("OPlfMounting", s"OP.mountLF", useOP)(),
      sS(
        sOnew(s"doLFMagic", s"Executor.$LFMagic", useUR)(
          c("post", "true", s"lf_pos := 'on_engine'")
        ),
        sOnew(s"delayBeforeHome", s"TON.delay", useTON)(cond("pre", "true", "TON.pt := 1000")),
        sOnew(s"resetBeforeHome", s"UR.pose.reset", useTON)(cond("pre", "true")),
        sOnew("toHomeAfterLFMagic1", s"UR.pose.goto_AfterLFOperationJOINT", useUR)(),
        sOnew("toHomeAfterLFMagic2", s"UR.pose.goto_PreHomeJOINT", useUR)(),
        sOnew("toHomeAfterLFMagicFinal", s"UR.pose.goto_HomeJOINT", useUR)()
      )
    )
  )






  /**
    * Detach LF and attach atlas
    * Tell op to add 6 pairs of screws
    * ******************************
    */


  o("log_in", s"OP.placeB1to6", useOP)(
    c("pre", "OP.loggedIn"),
    c("pre", s"lf_pos == 'on_engine'")
  )


  sop("detachLFToolWithExecutor")(
    c("pre", s"OP.loggedIn && $urPose == $HomeJOINT"),
    c("pre", s"urTool == 'lfTool'"),
    c("pre", s"lf_pos == 'on_engine'"),
    c("reset", "true")
  )(
    sOnew("toPreDetachLF", s"UR.pose.goto_PreAttachLFToolFarJOINT", useUR)(),
    sOnew(s"DetachLF", s"Executor.$DetachLFTool", useUR)(
      c("post", "true", s"urTool := 'none'")
    ),
    sOnew("toHomeAfterDetachLD", s"UR.pose.goto_HomeJOINT", useUR)()
  )



  sop("attachAtlasWithExecutor")(
    c("pre", s"$urPose == $HomeJOINT"),
    c("pre", s"lfPos == 'on_engine'"),
    c("pre", s"urTool == 'none'"),
    c("reset", "true")
  )(
    sOnew("toLifting", s"Atlas.lift", useUR)(),
    sOnew("toPreAttachAtlas", s"UR.pose.goto_PreAttachAtlasFarJOINT", useUR)(),
    sOnew("toFloating", s"Atlas.float", useUR)(),
    sOnew(s"AttachAtlasAfterLF", s"Executor.$AttachAtlas", useUR)(
      c("post", "true", s"urTool := 'atlas'")
    ),
    sOnew("toHomeAfterAtlas", s"UR.pose.goto_$AAPRAtlasTCP", useUR)(),
    sOnew("toHomeAfterAtlas2", s"UR.pose.goto_$AboveEngineTCP", useUR)(),
  )


  sop("detatchAtlasWithExecutor")(
    c("pre", s"allBolts && $urPose == $AAPRAtlasTCP"),
    c("pre", s"urTool == 'atlas'"),
    c("reset", "true")
  )(
    sOnew(s"detatchAtlasAfterBolt", s"Executor.$DetachAtlas", useUR ++ useRSP)(
      c("post", "true", s"urTool := 'none'")
    ),
    sOnew("toPreAttachAtlasAfterD", s"UR.pose.goto_PreAttachAtlasFarJOINT", useUR)(),
    sOnew("toHomeAfterBolting", s"UR.pose.goto_HomeJOINT", useUR)()
  )


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

//  sop("attachOFToolWithExecutor")(
//    c("pre", s"$urPose == $HomeJOINT"),
//    c("pre", s"urTool == 'none'"),
//    c("pre", s"filter1 == 'placed'")
//  )(
//    sOnew("toPreAttachLF", s"UR.pose.goto_$PreAttachOFToolFarJOINT", useUR)(),
//    sOnew(s"AttachLFAfterMeasure", s"Executor.$AttachOFTool", useUR)(
//      c("post", "true", s"urTool := 'ofTool'")
//    ),
//    sOnew("toFilterAfterLF1", s"UR.pose.goto_$OFMidpoint1JOINT", useUR)()
//  )





  /**
    * The bolt operations
    * ******************************
    */

//  sop("theBoltingSequence")(
//    c("pre", s"lf_pos == 'on_engine' && $urPose == $HomeJOINT"),
//    c("pre", s"urTool == 'atlas'")
//  )(
//    sP( // running some parallel ops.
//      sS(
//        sOnew("BeforeBoltingGotoAboveEngineTCP", s"UR.pose.goto_AboveEngineTCP", useUR)(),), // the UR sequence
//      sP() // support operations
//    )
//
//    sOnew(s"AttachLFAfterMeasure", s"Executor.$AttachLFTool", useUR)(
//      c("post", "true", s"urTool := 'lfTool'")
//    ),
//    sOnew("toHomeAfterLF", s"UR.pose.goto_HomeJOINT", useUR)(),
//  )

//  // goto above engine joint, the starting point for going to above bolt tcp poses
//  o(s"gotoAboveEngineTCP", s"UR.pose.goto_AboveEngineTCP")(
//    c("pre", s"$urPose == $HomeJOINT && urTool == 'on_engine'"),
//    c("reset", "true"))
//
  val boltUr = c("pre", "boltMode == 'ur' && urTool == 'atlas'")
  val boltHuman = c("pre", "boltMode == 'human' && (urTool != 'atlas' || urMode == 'float')")

  // go down from far above to nutrunner position and nutrunning, then back up
  bolts.foreach { b =>

    o(s"${b}goto${closeAboveBolt(b)}", s"UR.pose.goto_${closeAboveBolt(b)}")(
      c("pre", s"$urPose == '${farAboveBolt(b)}' && $b == 'placed'"),
      boltUr,
      c("reset", "true"))

    o(s"${b}goto${atBolt(b)}", s"UR.pose.goto_${atBolt(b)}")(
      c("pre", s"$urPose == '${closeAboveBolt(b)}' && $b == 'placed'"),
      boltUr,
      c("reset", "true"))

    o(s"${b}Tighten", "Atlas.startToolForward")(
      c("pre", s"$urPose == '${closeAboveBolt(b)}' && $b == 'placed'"),
      boltUr,
      c("post", "true", s"$b := 'tightened'"),
      c("reset", "true"))


    o(s"${b}backUpTo${farAboveBolt(b)}", s"UR.pose.goto_${farAboveBolt(b)}")(
      c("pre", s"$urPose == '${atBolt(b)}' && $b == 'tightened'"),
      boltUr,
      c("reset", "true"))

     o(s"${b}HumanTightenMotion")(//, s"Human.tightenMotion$b")(
       c("pre", s"$b == 'placed'"), boltHuman,
       c("post", "true", s"urPose := 'atTCPnut$b'"),
       c("reset", "true"))
  }
//
//  // sequence, from aboveEngine to nut 1..2..3..n.. back to aboveEngine
  val bm = bolts.zipWithIndex.map{case (b,i) => i->b}.toMap
  bm.foreach {
    case (0, b) => // FIRST
      o(s"${b}goto${farAboveBolt(b)}", s"UR.pose.goto_${farAboveBolt(b)}")(
        c("pre", s"$urPose == $AboveEngineTCP && $b == 'placed'"),
        boltUr,
        c("reset", "true"))

    case (i, b) => // OTHERS
      val prev = bm(i-1)
      o(s"${b}goto${farAboveBolt(b)}", s"UR.pose.goto_${farAboveBolt(b)}")(
        c("pre", s"$urPose == '${farAboveBolt(prev)}' && $b == 'placed' && $prev == 'tightened'"),
        boltUr,
        c("reset", "true"))
  }

  val lastB = bolts.last
  o(s"${farAboveBolt(lastB)}toAboveEngine", "UR.pose.goto_AboveEngineTCP")(
    c("pre", s"$urPose == '${farAboveBolt(lastB)}' && $lastB == 'tightened'"),
    c("reset", "true"))



  /**
    * Picking and leaving the LF tool
    * ******************************
    */

//  val noTool = c("pre", s"urTool == 'none'")
//  val reserve = c("pre", "urReserve == false", "urReserve := true")
//  val release = c("post", "true", "urReserve := false")
//
//
//  o(s"DetachOFTool", s"Executor.DetachOFTool", useUR)(
//    c("pre", s"$urPose == $HomeJOINT"),
//    c("pre", s"urTool == 'filterTool'"),
//    c("post", "true", s"urTool := 'none'"),
//    c("reset", "true"))
//
//
//  o(s"gotoPreAttachLFToolFarJOINT", s"UR.pose.goto_PreAttachLFToolFarJOINT", useUR)(
//    c("pre", s"$urPose == $HomeJOINT"),
//    c("pre", s"lf_pos == 'on_kitting'"),
//    noTool,
//    c("reset", "true"))
//
//  // goto close pre attach pose for the lf tool
//  o(s"gotoPreAttachLFToolCloseTCP", s"UR.pose.goto_PreAttachLFToolCloseTCP", useUR)(
//    c("pre", s"$urPose == $PreAttachLFToolFarJOINT"),
//    c("pre", s"(lf_pos == 'on_kitting' && urTool == 'none') || (lf_pos == 'on_engine' && urTool == 'lfTool')"),
//    c("reset", "true"))
//
//  //unlock RrsSP connector before attaching LF tool
//  o(s"releaseRspLfTool", s"RECU.unlock_rsp", useRSP)(
//    c("pre", s"$urPose == $PreAttachLFToolCloseTCP" ),
//    noTool,
//    c("reset", "true"))
//
//  // goto attach pose for the lf tool
//  o(s"gotoAttachLFToolTCP", s"UR.pose.goto_AttachLFToolTCP", useUR)(
//    c("pre", s"$urPose == $PreAttachLFToolCloseTCP"),
//    c("pre", s"lf_pos == 'on_kitting'"),
//    noTool,
//    c("reset", "true"))
//
//  //lock RrsSP connector for the LF tool
//  o(s"attachRspLfTool", s"RECU.lock_rsp", useRSP)(
//    c("pre", s"$urPose == $AttachLFToolTCP" ),
//    noTool,
//    c("post", "true", s"urTool := lfTool"),
//    c("reset", "true")
//  )













  runner("runner")

  // share a single driver for all ROS nodes
  // driver("ROSdriver", ROSFlatStateDriver.driverType)
}
