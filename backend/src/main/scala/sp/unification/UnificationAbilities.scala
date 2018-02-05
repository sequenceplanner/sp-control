package sp.unification

import akka.actor._
import sp.domain._
import sp.domain.Logic._
import java.util.UUID
import akka.cluster.pubsub.DistributedPubSub
import akka.cluster.pubsub.DistributedPubSubMediator.{ Put, Subscribe, Publish }
import scala.util.{Failure, Success, Try}
import sp.domain.logic.{PropositionParser, ActionParser}

import sp.abilityhandler.APIAbilityHandler
import sp.devicehandler.APIVirtualDevice

object UnificationAbilities {
  def props(ahid: ID) = Props(classOf[UnificationAbilities], ahid)
}

class UnificationAbilities(ahid: ID) extends Actor {
  import context.dispatcher
  val mediator = DistributedPubSub(context.system).mediator

  // The resource state
  // i'm using hardcoded IDs to simplify things while testing. We need a clear model message before removing this
  val refPos = Thing(name = "refPos", id = ID.makeID("c9442da3-1360-4e42-b618-f1c00d4e0212").get)
  val active = Thing(name = "active", id = ID.makeID("47a8200d-c402-4bd4-ace8-dfdcd97b89d1").get)
  val hasTool = Thing("hasTool", id = ID.makeID("b73dcd44-c7de-4ea0-9574-b35e9c2e500c").get)
  // can not change (currently we do not distinguish)
  val currentPos = Thing("currentPos", id = ID.makeID("c4a62002-e646-4659-9ee7-94d54ef1cd48").get)

  val things = List(refPos, active, hasTool, currentPos)
  val ids = things.map(_.id).toSet


  // abilities



  // resource and driver setup
  // This should be moved out from this and a model should be created soon.
  val driverName = "URDriver"
  val driver = sp.devicehandler.APIVirtualDevice.Driver("URDriver1", ID.newID, driverName, SPAttributes())

  val driverResourceMapper = List(
    sp.devicehandler.APIVirtualDevice.OneToOneMapper(refPos.id, driver.id, "refPos"),
    sp.devicehandler.APIVirtualDevice.OneToOneMapper(active.id, driver.id, "active"),
    sp.devicehandler.APIVirtualDevice.OneToOneMapper(hasTool.id, driver.id, "hasTool"),
    sp.devicehandler.APIVirtualDevice.OneToOneMapper(currentPos.id, driver.id, "currentPos")
  )
  val resource = sp.devicehandler.APIVirtualDevice.Resource("DummyUR", ID.newID, ids, driverResourceMapper, SPAttributes())

  val vd = SPSpec("VirtualDeviceURDummy", SPAttributes(
    "specType" -> "virtualDevice",
    "drivers" -> SPValue(List(driver)),
    "resources" -> SPValue(List(resource))
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
