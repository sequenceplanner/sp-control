package sp.labkit

// FESTO abilities
import akka.actor._
import sp.domain._
import sp.domain.Logic._
import java.util.UUID
import akka.cluster.pubsub.DistributedPubSub
import akka.cluster.pubsub.DistributedPubSubMediator.{ Put, Subscribe, Publish }
import scala.util.{Failure, Success, Try}
import sp.domain.logic.{PropositionParser, ActionParser}

import sp.abilityhandler.APIAbilityHandler
import sp.devicehandler._


object Festo {
  def props(ahid: ID) = Props(classOf[Festo], ahid)
}



//TODO: Need to update this so it makes a model instead

class Festo(ahid: ID) extends Actor with Helpers {
  import context.dispatcher
  val mediator = DistributedPubSub(context.system).mediator

  val pi10vars = List(
    v("button", "ButtonOn10"),
    v("motor", "MotorOn10")
  )

  val pi50vars = List(
    v("button", "ButtonOn50"),
    v("motor", "MotorOn50")
  )

  def p10(cond: String,actions: List[String] = List()) = prop(pi10vars)(cond,actions)
  def p50(cond: String,actions: List[String] = List()) = prop(pi50vars)(cond,actions)

  val pi10abs = List(
    a("pi10_startMotor",
      p10("!motor", List("motor := true")),
      p10("motor"),
      p10("button", List("motor := false")), Map("device" -> "pi10", "group"->"motor", "type"->"start"))
  )

  val pi50abs = List(
    a("pi50_startMotor",
      p50("!motor", List("motor := true")),
      p50("motor"),
      p50("button", List("motor := false")), Map("device" -> "pi50","group"->"motor", "type"->"start"))
  )

  val abs = pi10abs ++ pi50abs

  // setup pi10 driver
  val pi10url = "opc.tcp://129.16.37.101:10010"
  val driverID10 = UUID.randomUUID()
  val nodeprefix10 = "|var|CODESYS Control for Raspberry Pi SL.Application.POU."
  def sm10(vars: List[Thing]): List[VD.OneToOneMapper] = vars.flatMap { v =>
    v.attributes.getAs[String]("drivername").map(dn => VD.OneToOneMapper(v.id, driverID10, nodeprefix10+dn))
  }
  val setup10 = SPAttributes("url" -> pi10url, "identifiers" -> sm10(pi10vars).map(_.driverIdentifier))
  val driver10 = VD.Driver("opclocal", driverID10, "OPCUA", setup10)
  mediator ! Publish(APIDeviceDriver.topicRequest, SPMessage.makeJson[SPHeader, APIDeviceDriver.SetUpDeviceDriver](SPHeader(from = "hej"), APIDeviceDriver.SetUpDeviceDriver(driver10)))

  // setup pi50 driver
  val pi50url = "opc.tcp://129.16.37.101:10050"
  val driverID50 = UUID.randomUUID()
  val nodeprefix50 = "|var|CODESYS Control for Raspberry Pi SL.Application.POU."
  def sm50(vars: List[Thing]): List[VD.OneToOneMapper] = vars.flatMap { v =>
    v.attributes.getAs[String]("drivername").map(dn => VD.OneToOneMapper(v.id, driverID50, nodeprefix50+dn))
  }
  val setup50 = SPAttributes("url" -> pi50url, "identifiers" -> sm50(pi50vars).map(_.driverIdentifier))
  val driver50 = VD.Driver("opclocal", driverID50, "OPCUA", setup50)
  mediator ! Publish(APIDeviceDriver.topicRequest, SPMessage.makeJson[SPHeader, APIDeviceDriver.SetUpDeviceDriver](SPHeader(from = "hej"), APIDeviceDriver.SetUpDeviceDriver(driver50)))

  // setup resources
  val pi10 = VD.Resource("pi10", UUID.randomUUID(), pi10vars.map(_.id).toSet, sm10(pi10vars), SPAttributes())
  val pi50 = VD.Resource("pi50", UUID.randomUUID(), pi50vars.map(_.id).toSet, sm50(pi50vars), SPAttributes())

  val resources = List(pi10, pi50)

//  resources.foreach { res =>
//    val body = APIDeviceDriver.SetUpResource(res)
//    mediator ! Publish(APIDeviceDriver.topicRequest, SPMessage.makeJson[SPHeader, APIVirtualDevice.SetUpResource](SPHeader(from = "hej"), body))
//  }

  // setup abilities
  abs.foreach { ab =>
    val body = APIAbilityHandler.SetUpAbility(ab)
    val msg = SPMessage.makeJson[SPHeader, APIAbilityHandler.SetUpAbility](SPHeader(to = ahid.toString, from = "hej"), body)
    mediator ! Publish(APIAbilityHandler.topicRequest, msg)
  }

  // operations test
  def hAtt(h: String) = SPAttributes("hierarchy" -> Set(h))
  def ab(name: String) = SPAttributes("ability" -> name)

  def receive = {
    case x => println(x)
  }

}
