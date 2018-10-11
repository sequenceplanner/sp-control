package sp.drivers.ros2

import sp.domain._
import sp.domain.Logic._
import sp.modelSupport._


trait ROS2ModelSupport extends ModelDSL {
  def writer(driver: String, messageType: String, topic: String, rate: Int) = {
  }
  def publish(driver: String, messageType: String, topic: String, rate: Int) = {
    val emptyMsg = ROSHelpers.createROSMsg(messageType).get // puke if we mis-spell
    val attr = ROSHelpers.msgToAttr(emptyMsg)
    attr.value.foreach {
      case (field, nested: SPAttributes) =>
      // TODO: later
      case (field, value) =>
        val ident = "pub:" + messageType + ":" + topic + ":" + field + ":" + rate.toString
        dv(field, driver, ident, WriteOnly)
        v(field, value, List()) // add mapped variable with initial state to own the state
    }
  }
  def reader(driver: String, messageType: String, topic: String) = {
  }
  def subscribe(driver: String, messageType: String, topic: String) = {
    val emptyMsg = ROSHelpers.createROSMsg(messageType).get // puke if we mis-spell
    val attr = ROSHelpers.msgToAttr(emptyMsg)
    attr.value.foreach {
      case (field, nested: SPAttributes) =>
      // TODO: later
      case (field, v) =>
        val ident = "sub:" + messageType + ":" + topic + ":" + field
        dv(field, driver, ident, ReadOnly)
    }
  }
}
