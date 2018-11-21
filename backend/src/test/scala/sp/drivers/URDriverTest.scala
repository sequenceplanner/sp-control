package sp.drivers

import akka.actor._
import akka.cluster.pubsub.DistributedPubSubMediator.{Publish, Subscribe}
import akka.cluster.pubsub._
import akka.testkit._
import com.typesafe.config._
import org.scalatest._
// import sp.devicehandler._
import sp.domain.Logic._
import sp.domain._

import scala.concurrent.duration._

// TODO: Test crashes test runs. Needs to be fixed to be uncommented.
/*
/**
 * Testing AbilityActor
 */
class URDriverTest(_system: ActorSystem) extends TestKit(_system) with ImplicitSender
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
      val handler = system.actorOf(URDriver.props)

  }

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }



  "dummyUR testing" - {
    "activate" in {
      val p = TestProbe()
      val dummy = system.actorOf(DummyUR.props(p.ref))
      dummy ! "activate"

      p.fishForMessage(1 second){
        case URStream(_, _, true, _) => true
        case x => println(x); false
      }
    }

    "moving" in {
      val p = TestProbe()
      val dummy = system.actorOf(DummyUR.props(p.ref))
      dummy ! "activate"
      dummy ! 10


      p.fishForMessage(10 second){
        case URStream(3, 10, _, _) => true
        case x: URStream => println(x); false
      }
    }

    "deactivate" in {
      val p = TestProbe()
      val dummy = system.actorOf(DummyUR.props(p.ref))
      var z = 0
      dummy ! "activate"

      p.fishForMessage(10 second){
        case URStream( _, _, true, _) =>
          dummy ! "deactivate"
          z = 1
          false
        case URStream( _, _, false, _) if z == 1 => true // => z == 1
        case x: URStream => println(x); false
      }
    }

  }


  "DummyURDriverRuntime" - {
    "initial creation" in {
      val driverID = ID.newID
      val d = VD.Driver("test", driverID, "URDriver", SPAttributes())
      val setup = APIDeviceDriver.SetUpDeviceDriver(d)
      sendMess(setup)

      val p = TestProbe()
      mediator ! Subscribe("driverEvents", p.ref)
      val cmd = APIDeviceDriver.DriverCommand(driverID, Map("active"->true))


      p.fishForMessage(1 second){
        case x: String =>
          val spmess = SPMessage.fromJson(x)
          println(spmess)

          // The driver has started, send the command
          spmess.map{ _.getBodyAs[APIDeviceDriver.Response].collect {
              case l: APIDeviceDriver.TheDriver => sendMess(cmd, driverID)
            }
          }

          // The command was written to the dummy
          spmess.flatMap{ _.getBodyAs[APIDeviceDriver.Request].collect {
            case l: APIDeviceDriver.DriverCommandDone => l.requestID == driverID
          }}.getOrElse(false)

      }
    }

    "it is moving" in {
      val driverID = ID.newID
      val d = VD.Driver("test", driverID, "URDriver", SPAttributes())
      val setup = APIDeviceDriver.SetUpDeviceDriver(d)
      sendMess(setup)

      val p = TestProbe()
      mediator ! Subscribe("driverEvents", p.ref)
      val mess = APIDeviceDriver.DriverCommand(driverID, Map("active"->true, "refPos" -> 2))
      // TODO: Update the test to wait with the cmd until after the driver is loaded
      sendMess(mess, driverID)

      p.fishForMessage(1 second){
        case x: String =>
          println(x)
          val r = SPMessage.fromJson(x).flatMap{ mess =>
            for {
              h <- mess.getHeaderAs[SPHeader]
              b <- mess.getBodyAs[sp.devicehandler.APIDeviceDriver.Request] if b.isInstanceOf[APIDeviceDriver.DriverStateChange]
              driver = b.asInstanceOf[APIDeviceDriver.DriverStateChange]
              if driver.id == driverID
              x <- driver.state.get("currentPos")
              y <-   x.to[Int].toOption
            } yield {
              y > 1
            }
          }
          r.getOrElse(false)
      }
    }

  }


  def sendMess(x: APIDeviceDriver.Request, reqID: ID = ID.newID) = {
    val toSend = SPMessage.makeJson(SPHeader(from = "testing", reqID = reqID), x)
    mediator ! Publish("driverCommands", toSend)
  }




}


*/
