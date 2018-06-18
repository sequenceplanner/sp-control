package sp.drivers

import akka.actor._
import akka.cluster.pubsub.DistributedPubSubMediator.{Publish, Subscribe}
import akka.cluster.pubsub._
import akka.testkit._
import com.typesafe.config._
import org.scalatest._
import sp.devicehandler._
import sp.domain.Logic._
import sp.domain._
import sp.driver.APIHumanDriver

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
      |akka.loglevel = "NONE"
      |akka.actor.provider = "akka.cluster.ClusterActorRefProvider"
      |akka.remote.netty.tcp.hostname="127.0.0.1"
      |akka.remote.netty.hostname.port=2558
      |akka.cluster.seed-nodes=["akka.tcp://SP@127.0.0.1:2558"]
    """.stripMargin)))

  val mediator = DistributedPubSub(system).mediator
  val id = ID.newID


  override def beforeAll: Unit = {
      val handler = system.actorOf(HumanDriver.props)

  }

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }


  "bluetooth comm test" - {
    "testing proxy setup" in {
      val p = new Proxy

    }
  }


  "HumanDriverRuntime" - {
    "initial creation" in {
      val driverID = ID.newID
      val reqID = ID.newID
      val d = VD.Driver("kristofer", driverID, "HumanDriver", SPAttributes())
      val setup = APIDeviceDriver.SetUpDeviceDriver(d)
      sendMess(setup)

      val p = TestProbe()
      val human = system.actorOf(Props(classOf[HumanMockUP]))
      mediator ! Subscribe(APIDeviceDriver.topicResponse, p.ref)

      val cmd = APIDeviceDriver.DriverCommand(driverID, Map("cmd"->true))


      p.fishForMessage(10 second){
        case x: String =>
          val spmess = SPMessage.fromJson(x)
          println(spmess)

          // The driver has started, send the command
          spmess.map{ _.getBodyAs[APIDeviceDriver.Response].collect {
              case l: APIDeviceDriver.TheDriver => sendMess(cmd, reqID)
            }
          }

          // The command was written to the dummy
          spmess.flatMap{ _.getBodyAs[APIDeviceDriver.Response].collect {
            case l: APIDeviceDriver.DriverCommandDone => l.requestID == reqID
          }}.getOrElse(false)

      }
    }

  }


  def sendMess(x: APIDeviceDriver.Request, reqID: ID = ID.newID) = {
    val toSend = SPMessage.makeJson(SPHeader(from = "testing", reqID = reqID), x)
    mediator ! Publish(APIDeviceDriver.topicRequest, toSend)
  }


}

class HumanMockUP extends Actor
  with ActorLogging
  with sp.service.MessageBussSupport {

  subscribe(APIHumanDriver.topicToHuman)

  override def receive = {
    case x: String =>
      println("The human got: "+ x)
      for {
        mess <- SPMessage.fromJson(x)
        h <- mess.getHeaderAs[SPHeader]
        b <- mess.getBodyAs[APIHumanDriver.ToHuman]
      } yield {
        val updH = h.swapToAndFrom
        val state = b match {
          case APIHumanDriver.StateChangeRequest(name, s) => s
        }
        publish(APIHumanDriver.topicFromHuman, SPMessage.makeJson(
          updH, APIHumanDriver.HumanEvent("kristofer", state ++ Map("key1" -> SPValue("hej")))
        ))
      }
    case x => println("HUMAN MOCKUP GOT: " + x)
  }

}


