package sp.drivers.ros2

import sp.domain._
import sp.domain.Logic._
import sp.modelSupport._


trait ROS2ModelSupport extends ModelDSL {
  def writer(driver: String, messageType: String, topic: String, rate: Int) = {
    // val emptyMsg = ROSHelpers.createROSMsg(messageType).get // puke if we mis-spell
    // val attr = ROSHelpers.ROSMsgToSPAttributes(emptyMsg).get // puke if we can't parse
    // attr.value.foreach {
    //   case (field, nested: SPAttributes) =>
    //   // TODO: later
    //   case (field, v) =>
    //     val ident = messageType + ":" + topic + ":" + field + ":" + rate
    //     dv(field, driver, ident, WriteOnly)
    // }
  }
  def reader(driver: String, messageType: String, topic: String) = {
    // val emptyMsg = ROSHelpers.createROSMsg(messageType).get // puke if we mis-spell
    // val attr = ROSHelpers.ROSMsgToSPAttributes(emptyMsg).get // puke if we can't parse
    // attr.value.foreach {
    //   case (field, nested: SPAttributes) =>
    //   // TODO: later
    //   case (field, v) =>
    //     val ident = messageType + ":" + topic + ":" + field
    //     dv(field, driver, ident, ReadOnly)
    // }
  }
}
