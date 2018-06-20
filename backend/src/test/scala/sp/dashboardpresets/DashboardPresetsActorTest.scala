package sp.dashboardpresets

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, FreeSpecLike, Matchers, WordSpecLike}
import sp.{APIDashboardPresets => API}

class DashboardPresetsActorTest(_system: ActorSystem) extends TestKit(_system) with WordSpecLike with Matchers with BeforeAndAfterAll {

  val prober = TestProbe("Tester")
  val actor: ActorRef = system.actorOf(DashboardPresetsActor(Some(prober.ref)))

  "State modifications" must {
    "Not modify state in request" in {
      actor ! API.DashboardPresetsRequest
      prober.expectMsg(Map[String, String]())
    }
  }
}
