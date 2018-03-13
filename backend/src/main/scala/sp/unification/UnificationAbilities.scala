package sp.unification

import akka.actor._
import sp.domain._
import sp.domain.Logic._
import java.util.UUID

import akka.cluster.pubsub.DistributedPubSub
import akka.cluster.pubsub.DistributedPubSubMediator.{Publish, Put, Subscribe}

import scala.util.{Failure, Success, Try}
import sp.domain.logic.{ActionParser, PropositionParser}
import sp.abilityhandler._
import sp.devicehandler._
import sp.drivers.{HumanDriver, URDriver}

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
  //val working = Thing("working")
  //val humanRefPos = Thing("humanRefPos")
  //val humanCurrentPos = Thing("humanCurrentPos")


  // abilities
  val activate = sp.abilityhandler.APIAbilityHandler.Ability(
    name = "activate",
    preCondition = makeCondition("!active", "active := true"),
    postCondition = makeCondition("active")
  )

  val moveTo10 = sp.abilityhandler.APIAbilityHandler.Ability(
    name = "moveTo10",
    preCondition = makeCondition("active && refPos == currentPos && currentPos != 10", "refPos := 10"),
    started = makeCondition("refPos = 10"),
    postCondition = makeCondition("currentPos = 10")
  )


  val moveTo20 = sp.abilityhandler.APIAbilityHandler.Ability(
    name = "moveTo20",
    preCondition = makeCondition("active && refPos == currentPos && currentPos != 20", "refPos := 20"),
    started = makeCondition("refPos = 20"),
    postCondition = makeCondition("currentPos = 20")
  )
/*
  val humanMoveToStation = sp.abilityhandler.APIAbilityHandler.Ability(
    name = "humanMoveToStation",
    preCondition = makeCondition("!working && humanRefPos == humanCurrentPos && humanCurrentPos != 30", "humanRefPos := 30"),
    started = makeCondition("humanRefPos = 30"),
    postCondition = makeCondition("humanCurrentPos = 30")
  )

  val humanStartWorking = sp.abilityhandler.APIAbilityHandler.Ability(
    name = "humanStartWorking",
    preCondition = makeCondition("!working && humanRefPos == humanCurrentPos && humanCurrentPos == 30", "working := true"),
    started = makeCondition("working = true"),
    postCondition = makeCondition("working = true")
  )
*/


  val abs = List(activate, moveTo10, moveTo20)


q

  def makeCondition(guard: String, actions: String*) = {

    val things: List[Thing] = List(refPos, active, hasTool, currentPos)


    val g = PropositionParser(things).parseStr(guard) match {
      case Right(p) => Some(p)
      case Left(err) => println(s"Parsing failed on condition: $guard: $err"); None
    }

    val xs = actions.flatMap { action =>
      ActionParser(things).parseStr(action) match {
        case Right(a) => Some(a)
        case Left(err) => println(s"Parsing failed on action: $action: $err"); None
      }
    }
    Condition(g.get, xs.toList)
  }

  def prop(vars: List[IDAble])(cond: String,actions: List[String] = List()) = {
    def c(condition: String): Option[Proposition] = {
      PropositionParser(vars).parseStr(condition) match {
        case Right(p) => Some(p)
        case Left(err) => println(s"Parsing failed on condition: $condition: $err"); None
      }
    }

    def a(actions: List[String]): List[Action] = {
      actions.flatMap { action =>
        ActionParser(vars).parseStr(action) match {
          case Right(a) => Some(a)
          case Left(err) => println(s"Parsing failed on action: $action: $err"); None
        }
      }
    }
    Condition(c(cond).get, a(actions))
  }


  // resource and driver setup
  // This should be moved out from this and a model should be created soon.
  val driver1 = sp.devicehandler.APIVirtualDevice.Driver("URDriver1", ID.newID, URDriver.driverType, SPAttributes())
  //val driver2 = sp.devicehandler.APIVirtualDevice.Driver("HumanDriver1", ID.newID, HumanDriver.driverType, SPAttributes())

  val driverResourceMapper = List(
    sp.devicehandler.APIVirtualDevice.OneToOneMapper(refPos.id, driver1.id, "refPos"),
    sp.devicehandler.APIVirtualDevice.OneToOneMapper(active.id, driver1.id, "active"),
    sp.devicehandler.APIVirtualDevice.OneToOneMapper(hasTool.id, driver1.id, "hasTool"),
    sp.devicehandler.APIVirtualDevice.OneToOneMapper(currentPos.id, driver1.id, "currentPos"),
    //sp.devicehandler.APIVirtualDevice.OneToOneMapper(working.id, driver2.id, "working"),
    //sp.devicehandler.APIVirtualDevice.OneToOneMapper(humanRefPos.id, driver2.id, "humanRefPos"),
    //sp.devicehandler.APIVirtualDevice.OneToOneMapper(humanCurrentPos.id, driver2.id, "humanCurrentPos")

  )
  val ids1: Set[ID] = List(refPos, active, hasTool, currentPos).map(_.id).toSet
  //val ids2: Set[ID] = List(working, humanRefPos, humanCurrentPos).map(_.id).toSet

  val resourceUR = sp.devicehandler.APIVirtualDevice.Resource("DummyUR", ID.newID, ids1, driverResourceMapper, SPAttributes())
  //val resourceHuman = sp.devicehandler.APIVirtualDevice.Resource("DummyHuman", ID.newID, ids2, driverResourceMapper, SPAttributes())

//  TODO Fix the model
//  val vd = SPSpec("VirtualDeviceURDummy", SPAttributes(
//    "specType" -> "virtualDevice",
//    "drivers" -> List(SPValue(driver)),
//    "resources" -> List(SPValue(resource))
//  ))

  // Setting up the model
  val modelID = ID.makeID("0d80d1d6-48cd-48ec-bfb1-d69714ef35be").get // hardcoded model id so we do not get a new model every time


  mediator ! Publish(APIVirtualDevice.topicRequest,
    SPMessage.makeJson(
      SPHeader(from = "UnificationAbilities"),
      APIVirtualDevice.SetUpDeviceDriver(driver1)))
  //mediator ! Publish(APIVirtualDevice.topicRequest,
  //SPMessage.makeJson(
  //    SPHeader(from = "UnificationAbilities"),
  //    APIVirtualDevice.SetUpDeviceDriver(driver2)))
  mediator ! Publish(APIVirtualDevice.topicRequest,
    SPMessage.makeJson(
      SPHeader(from = "UnificationAbilities"), APIVirtualDevice.SetUpResource(resourceUR)))
  //mediator ! Publish(APIVirtualDevice.topicRequest,
  //SPMessage.makeJson(
  //    SPHeader(from = "UnificationAbilities"), APIVirtualDevice.SetUpResource(resourceHuman)))

  abs.foreach { ab =>
    val body = APIAbilityHandler.SetUpAbility(ab)
    val msg = SPMessage.makeJson[SPHeader, APIAbilityHandler.SetUpAbility](SPHeader(to = ahid.toString, from = "hej"), body)
    mediator ! Publish(APIAbilityHandler.topicRequest, msg)
  }

  // Not doing anything, creates the model on startup
  def receive = {
    case _ =>
  }

}
