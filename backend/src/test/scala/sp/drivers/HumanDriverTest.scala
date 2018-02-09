package sp.drivers

import akka.actor._
import akka.cluster.pubsub.DistributedPubSubMediator.{Publish, Subscribe}
import akka.cluster.pubsub._
import akka.testkit._
import com.typesafe.config._
import org.scalatest._
import sp.devicehandler.APIVirtualDevice
import sp.domain.Logic._
import sp.domain._

import scala.concurrent.duration._


/**
 * Testing AbilityActor
 */
class HumanDriverTest(_system: ActorSystem) extends TestKit(_system) with ImplicitSender
  with FreeSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("SP", ConfigFactory.parseString(
    """
      |akka.persistence.journal.plugin = "akka.persistence.journal.inmem"
      |akka.persistence.snapshot-store.plugin = "akka.persistence.snapshot-store.local"
      |akka.persistence.snapshot-store.local.dir = "target/snapshotstest/"
      |akka.loglevel = "INFO"
      |akka.actor.provider = "akka.cluster.ClusterActorRefProvider"
      |akka.remote.netty.tcp.hostname="127.0.0.1"
      |akka.remote.netty.hostname.port=2551
      |akka.cluster.seed-nodes=["akka.tcp://SP@127.0.0.1:2551"]
    """.stripMargin)))

  val mediator = DistributedPubSub(system).mediator
  val id = ID.newID


  override def beforeAll: Unit = {
      val handler = system.actorOf(HumanDriver.props)

  }

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }


/*
  "dummyHuman testing" - {
    "moving" in {
      val p = TestProbe()
      val dummy = system.actorOf(DummyHuman.props(p.ref))
      dummy ! 10
      println("moving to 10")

      p.fishForMessage(10 second){
        case HumanStream( 10, 10, false) => true
        case x: HumanStream => println(x); false
      }
    }


  }
  */

  "job2" in {
      val p = TestProbe()
      val dummy = system.actorOf(DummyHuman.props(p.ref))
      var jobDone = false

      p.fishForMessage(10 second){
        case HumanStream( 20, 20, false) if !jobDone =>
          dummy ! "moveToStation";
          false
        case HumanStream(30, 30, false) if !jobDone =>
          dummy ! "startWorking";
          false
        case HumanStream( 30, 30, true) if !jobDone =>
          dummy ! "stopWorking";
          jobDone = true
          false
        case HumanStream( 30, 30, false) if jobDone =>
          dummy ! "moveToHome";
          false
        case HumanStream( 20, 20, false) if jobDone =>
          println("jobDone");
          true

        case x: HumanStream => println(x); false
      }
    }



  def sendMess(x: APIVirtualDevice.Request, reqID: ID = ID.newID) = {
    val toSend = SPMessage.makeJson(SPHeader(from = "testing", reqID = reqID), x)
    mediator ! Publish("driverCommands", toSend)
  }




}


