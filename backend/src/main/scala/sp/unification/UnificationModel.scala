package sp.unification

import sp.modelSupport._
import sp.domain.Logic._
import sp.domain._
import sp.devicehandler._

import sp.drivers.ROSFlatStateDriver
import sp.drivers.ROSHelpers

trait ROSSupport extends ModelDSL {
  def writer(driver: String, messageType: String, topic: String, rate: Int) = {
    val emptyMsg = ROSHelpers.createROSMsg(messageType).get // puke if we mis-spell
    val attr = ROSHelpers.ROSMsgToSPAttributes(emptyMsg).get // puke if we can't parse
    attr.value.foreach {
      case (field, nested: SPAttributes) =>
      // TODO: later
      case (field, v) =>
        val ident = messageType + ":" + topic + ":" + field + ":" + rate
        dv(field, driver, ident, WriteOnly)
    }
  }
  def reader(driver: String, messageType: String, topic: String) = {
    val emptyMsg = ROSHelpers.createROSMsg(messageType).get // puke if we mis-spell
    val attr = ROSHelpers.ROSMsgToSPAttributes(emptyMsg).get // puke if we can't parse
    attr.value.foreach {
      case (field, nested: SPAttributes) =>
      // TODO: later
      case (field, v) =>
        val ident = messageType + ":" + topic + ":" + field
        dv(field, driver, ident, ReadOnly)
    }
  }
}

class UnificationModel extends ModelDSL {
  //use("UR", new UR)
  use("Atlas", new Atlas)
  // use("MiR", new MiR)

  // runner (TODO: for now runners take everything and must be on the top level of the model)




  // MAIN MODEL

  // products
  val bolts = (1 to 10).map(i => s"boltPair$i")

  v("lf_pos", "on_kitting", List("on_kitting", "on_engine"))
  bolts.foreach { b => v(b, "empty", List("empty", "placed", "tightened")) }
  v("filter1", "empty", List("empty", "placed", "tightened"))
  v("filter2", "empty", List("empty", "placed", "tightened"))
  v("pipes", "empty", List("empty", "placed"))

  // resources
  v("boltMode" ,"ur", List("ur", "human"))

  v("urMode", "running", List("running", "float", "stopped"))
  v("urTool", "none", List("none", "lfTool", "atlas", "filterTool"))
  v("urPose", "HOME", (List("HOME",
    "atLfTool", "atAtlas", "atFilterTool") ++
    bolts.map(b => s"at$b") ++ bolts.map(b => s"atTCP$b") ++ bolts.map(b => s"atTCPnut$b") ++
    List("atFilter1", "atFilter2")).map(s=>SPValue(s)))


  // operations

  bolts.foreach { b =>
    val boltUr = c("pre", "boltMode == 'ur' && urTool == 'atlas'")
    val boltHuman = c("pre", "boltMode == 'human' && (urTool != 'atlas' || urMode == 'float')")

    o(s"${b}Tighten")(//, s"UR.tighten$b")(
      c("pre", s"urPose == 'atTCP$b' && $b == 'placed'"), boltUr,
      c("post", "true", s"$b := 'tightened'"),
      c("reset", "true"))

    o(s"${b}TightenMotion")( //, s"UR.tightenMotion$b")(
      c("pre", s"urPose == 'atTCP$b' && $b == 'placed'"), boltUr,
      c("post", "true", s"urPose := 'atTCPnut$b'"),
      c("reset", "true"))

    o(s"${b}upToTCP")(///, s"UR.atToTCP$b")(
      c("pre", s"urPose == 'atTCPnut$b' && $b == 'tightened'"), boltUr,
      c("post", "true", s"urPose := 'atTCP$b'"),
      c("reset", "true"))

    o(s"${b}HumanTightenMotion")(//, s"Human.tightenMotion$b")(
      c("pre", s"$b == 'placed'"), boltHuman,
      c("post", "true", s"urPose := 'atTCPnut$b'"),
      c("reset", "true"))
  }

  val bm = bolts.zipWithIndex.map{case (b,i) => i->b}.toMap
  bm.map {
    case (0, b) =>
      o(s"to${b}")(
        c("pre", "boltMode == 'ur' && urTool == 'atlas'"),
        c("pre", s"$b == 'placed'"),
        c("post", "true", s"urPose := 'at$b'"),
        c("reset", "true"))
    case (i, b) =>
      val prev = bm(i-1)
      o(s"from${prev}to${b}")(
        c("pre", "boltMode == 'ur' && urTool == 'atlas'"),
        c("pre", s"urPose == 'at$prev' && $b == 'placed'"),
        c("post", "true", s"urPose := 'at$b'"),
        c("reset", "true"))






  }


  runner("runner")

  // share a single driver for all ROS nodes
  driver("ROSdriver", ROSFlatStateDriver.driverType)
}


object UnificationModel {
  def apply() = new UnificationModel
}
