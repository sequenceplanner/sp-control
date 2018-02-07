package sp.unification

import akka.actor._
import sp.domain._
import sp.domain.Logic._
import java.util.UUID

import akka.cluster.pubsub.DistributedPubSub
import akka.cluster.pubsub.DistributedPubSubMediator.{Publish, Put, Subscribe}

import scala.util.{Failure, Success, Try}
import sp.domain.logic.{ActionParser, PropositionParser}
import sp.abilityhandler.APIAbilityHandler
import sp.devicehandler.APIVirtualDevice
import sp.drivers.URDriver

object UnificationAbilities {
  def props(ahid: ID) = Props(classOf[UnificationAbilities], ahid)
}

class UnificationAbilities(ahid: ID) extends Actor {
  import context.dispatcher
  val mediator = DistributedPubSub(context.system).mediator

  // The resource state
  // i'm using hardcoded IDs to simplify things while testing. We need a clear model message before removing this
  val refPos = Thing(name = "refPos")
  val active = Thing(name = "active")
  val hasTool = Thing("hasTool")
  // can not change (currently we do not distinguish)
  val currentPos = Thing("currentPos")

  val things = List(refPos, active, hasTool, currentPos)
  val ids = things.map(_.id).toSet


  // abilities



  // resource and driver setup
  // This should be moved out from this and a model should be created soon.
  val driver = sp.devicehandler.APIVirtualDevice.Driver("URDriver1", ID.newID, URDriver.driverType, SPAttributes())

  val driverResourceMapper = List(
    sp.devicehandler.APIVirtualDevice.OneToOneMapper(refPos.id, driver.id, "refPos"),
    sp.devicehandler.APIVirtualDevice.OneToOneMapper(active.id, driver.id, "active"),
    sp.devicehandler.APIVirtualDevice.OneToOneMapper(hasTool.id, driver.id, "hasTool"),
    sp.devicehandler.APIVirtualDevice.OneToOneMapper(currentPos.id, driver.id, "currentPos")
  )
  val resource = sp.devicehandler.APIVirtualDevice.Resource("DummyUR", ID.newID, ids, driverResourceMapper, SPAttributes())

  val vd = SPSpec("VirtualDeviceURDummy", SPAttributes(
    "specType" -> "virtualDevice",
    "drivers" -> List(SPValue (driver.asInstanceOf[APIVirtualDevice.Request])),
    "resources" -> List(SPValue(resource.asInstanceOf[APIVirtualDevice.Request]))
  ))

  // Setting up the model
  val modelID = ID.makeID("0d80d1d6-48cd-48ec-bfb1-d69714ef35be").get // hardcoded model id so we do not get a new model every time


  mediator ! Publish(APIVirtualDevice.topicRequest,
    SPMessage.makeJson(
      SPHeader(from = "UnificationAbilities"),
      APIVirtualDevice.SetUpDeviceDriver(driver)))
  mediator ! Publish(APIVirtualDevice.topicRequest,
    SPMessage.makeJson(
      SPHeader(from = "UnificationAbilities"), APIVirtualDevice.SetUpResource(resource)))


  // Not doing anything, creates the model on startup
  def receive = {
    case _ =>
  }

}
