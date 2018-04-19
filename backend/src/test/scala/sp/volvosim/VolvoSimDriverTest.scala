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
class VolvoSimDriverTest(_system: ActorSystem) extends TestKit(_system) with ImplicitSender
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
      val handler = system.actorOf(DummyVolvoRobotDriver.props)

  }

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }





  "DummyURDriverRuntime" - {
    "initial creation" in {
      val driverID = ID.newID
      val d = VD.Driver("test", driverID, DummyVolvoRobotDriver.driverType, SPAttributes())
      val setup = APIDeviceDriver.SetUpDeviceDriver(d)
      sendMess(setup)

      val p = TestProbe()
      mediator ! Subscribe(APIDeviceDriver.topicResponse, p.ref)
      val cmd = APIDeviceDriver.DriverCommand(driverID,
        Map("currentProgram"->"prog5")
      )


      p.fishForMessage(10 second){
        case x: String =>
          val spmess = SPMessage.fromJson(x)
          println(spmess)

          // The driver has started, send the command
          spmess.flatMap{ _.getBodyAs[APIDeviceDriver.Response].collect {
              case l: APIDeviceDriver.TheDriver =>
                sendMess(cmd, driverID)
                false

              case l: APIDeviceDriver.DriverCommandDone =>
                l.requestID == driverID
            }
          }.getOrElse(false)


      }
    }

    "running" in {
      val driverID = ID.newID
      val d = VD.Driver("test", driverID, DummyVolvoRobotDriver.driverType, SPAttributes())
      val setup = APIDeviceDriver.SetUpDeviceDriver(d)
      sendMess(setup)

      val p = TestProbe()
      mediator ! Subscribe(APIDeviceDriver.topicResponse, p.ref)
      val cmd = APIDeviceDriver.DriverCommand(driverID,
        Map("currentProgram"->"prog5")
      )



      var timeChanging = false
      p.fishForMessage(10 second){
        case x: String =>
          val spmess = SPMessage.fromJson(x)
          println(spmess)

          // The driver has started, send the command
          spmess.flatMap{ _.getBodyAs[APIDeviceDriver.Response].collect {
              case l: APIDeviceDriver.TheDriver =>
                sendMess(cmd, driverID)
                false

              case l: APIDeviceDriver.DriverCommandDone =>
                l.requestID == driverID
            }
          }.getOrElse(false)


      }
    }


  }


  def sendMess(x: APIDeviceDriver.Request, reqID: ID = ID.newID) = {
    val toSend = SPMessage.makeJson(SPHeader(from = "testing", reqID = reqID), x)
    mediator ! Publish(APIDeviceDriver.topicRequest, toSend)
  }




}


