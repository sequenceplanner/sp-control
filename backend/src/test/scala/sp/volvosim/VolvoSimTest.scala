package sp.volvosim

import akka.actor._
import akka.cluster.pubsub.DistributedPubSubMediator.{Publish, Subscribe}
import akka.cluster.pubsub._
import akka.testkit._
import com.typesafe.config._
import org.scalatest._
import sp.devicehandler._
import sp.domain.Logic._
import sp.domain._

import scala.concurrent.duration._


/**
 * Testing AbilityActor
 */
class VolvoSimTest(_system: ActorSystem) extends TestKit(_system) with ImplicitSender
  with FreeSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("SP", ConfigFactory.parseString(
    """
    """.stripMargin)))

  override def beforeAll: Unit = {

  }

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }



  "DummyRobot testing" - {
    "getting state" in {
      val p = TestProbe()
      val dummy = system.actorOf(DummyVolvoRobot.props(p.ref, Map()))

      p.fishForMessage(10 second){
        case DummyVolvoRobotState("", 0, false, true) =>
          true
        case x => println(x); false
      }
    }

    "running program" in {
      val p = TestProbe()
      val dummy = system.actorOf(DummyVolvoRobot.props(p.ref, Map("test" -> 5, "kalle"-> 10)))

      dummy ! DummyVolvoRobotStartProgram("test")

      var track = 0

      p.fishForMessage(10 second){
        case x @ DummyVolvoRobotState(prog, 0, false, true) if track == 2 =>
          println(x)
          println(track)
          true
        case x @ DummyVolvoRobotState(prog, 0, false, true) if track == 0 =>
          println(x)
          println(track)
          track = 1
          false
        case x @ DummyVolvoRobotState(prog, 4, false, true) if prog == "test" && track == 1 =>
          println(x)
          println(track)
          track = 2
          false

        case x => println("NEJ"); false
      }
    }



  }





}


