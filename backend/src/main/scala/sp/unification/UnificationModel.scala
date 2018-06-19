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
  use("UR", new UR)
  // use("MiR", new MiR)

  // runner (TODO: for now runners take everything and must be on the top level of the model)
  runner("runner")

  // share a single driver for all ROS nodes
  driver("ROSdriver", ROSFlatStateDriver.driverType)
}


object UnificationModel {
  def apply() = new UnificationModel
}
