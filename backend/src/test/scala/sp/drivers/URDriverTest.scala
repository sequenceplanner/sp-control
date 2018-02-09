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

    "deactivate" in {
      val p = TestProbe()
      val dummy = system.actorOf(DummyUR.props(p.ref))
      dummy ! "deactivate"

      p.fishForMessage(1 second){
        case URStream(_, _, false, _) => true
        case x => println(x); false
      }
    }


    // mini fsm test: job1:
    // robot activates, goes to position 3, gets the tool, goes to position 5,
    // picks up the item, goes to position 7, releases the item, goes to position 3,
    // releases the tool, robot deactivates

    "job1" in {
      val p = TestProbe()
      val dummy = system.actorOf(DummyUR.props(p.ref))
      var executed = false
      var transported = false

      p.fishForMessage(10 second){

        case URStream( 0, 0, false, false) =>
          dummy ! "activate";
          println("activated");
          false

        case URStream( 0, 0, true, false) =>
          dummy ! 3;
          println("moving to 3");
          false

        case URStream( 3, 3, true, false) if transported == false=>
          dummy ! "addTool";
          println("got the tool");
          false

        case URStream( 3, 3, true, true) if transported == false =>
          dummy ! 5;
          println("moving to 5");
          false

        case URStream( 5, 5, true, true) if transported == false =>
          dummy ! 7;
          println("item picked up");
          println("moving to 7");
          false

        case URStream( 7, 7, true, true) =>
          dummy ! 3;
          println("item placed");
          println("moving to 3");
          transported = true;
          false

        case URStream( 3, 3, true, true) if transported =>
          dummy ! "removeTool";
          println("released the tool");
          false

        case URStream( 3, 3, true, false) if transported =>
          dummy ! "deactivate";
          println("deactivated");
          executed = true;
          false

        case URStream( 3, 3, false, false) => executed
        case x => println(x); false
      }
    }

    "moving" in {
      val p = TestProbe()
      val dummy = system.actorOf(DummyUR.props(p.ref))
      dummy ! "activate"
      dummy ! 6

      p.fishForMessage(10 second){
        case URStream( 6, 6, _, _) => true
        case x: URStream => println(x); false
      }
    }

  }


  "DummyURDriverRuntime" - {
    "initial creation" in {
      val driverID = ID.newID
      val d = APIVirtualDevice.Driver("test", driverID, "URDriver", SPAttributes())
      val setup = APIVirtualDevice.SetUpDeviceDriver(d)
      sendMess(setup)

      val p = TestProbe()
      mediator ! Subscribe("driverEvents", p.ref)
      val cmd = APIVirtualDevice.DriverCommand("test", driverID, Map("active"->true))


      p.fishForMessage(1 second){
        case x: String =>
          val spmess = SPMessage.fromJson(x)
          println(spmess)
          spmess.map{ _.getBodyAs[APIVirtualDevice.Response].collect {
              case l: APIVirtualDevice.NewDriver => sendMess(cmd, driverID)
            }
          }

          spmess.flatMap{ _.getBodyAs[APIVirtualDevice.Request].collect {
            case l: APIVirtualDevice.DriverCommandDone => l.requestID == driverID
          }}.getOrElse(false)

      }
    }

    "it is moving" in {
      val driverID = ID.newID
      val d = APIVirtualDevice.Driver("test", driverID, "URDriver", SPAttributes())
      val setup = APIVirtualDevice.SetUpDeviceDriver(d)
      sendMess(setup)

      val p = TestProbe()
      mediator ! Subscribe("driverEvents", p.ref)
      val mess = APIVirtualDevice.DriverCommand("test", driverID, Map("active"->true, "refPos" -> 2))
      // TODO: Update the test to wait with the cmd until after the driver is loaded
      sendMess(mess, driverID)

      p.fishForMessage(1 second){
        case x: String =>
          println(x)
          val r = SPMessage.fromJson(x).flatMap{ mess =>
            for {
              h <- mess.getHeaderAs[SPHeader]
              b <- mess.getBodyAs[sp.devicehandler.APIVirtualDevice.Request] if b.isInstanceOf[APIVirtualDevice.DriverStateChange]
              driver = b.asInstanceOf[APIVirtualDevice.DriverStateChange]
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


  def sendMess(x: APIVirtualDevice.Request, reqID: ID = ID.newID) = {
    val toSend = SPMessage.makeJson(SPHeader(from = "testing", reqID = reqID), x)
    mediator ! Publish("driverCommands", toSend)
  }




}


