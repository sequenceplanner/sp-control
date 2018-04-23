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
      val handler2 = system.actorOf(VolvoTransportSimulationDriver.props)
      val handler3 = system.actorOf(VolvoPressureSimulationDriver.props)

  }

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }





  "DummyURDriverRuntime" - {
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



      var done = false
      p.fishForMessage(10 second){
        case x: String =>
          val spmess = SPMessage.fromJson(x)
          println("robot: " + spmess)

          // The driver has started, send the command
          spmess.flatMap{ _.getBodyAs[APIDeviceDriver.Response].collect {
              case l: APIDeviceDriver.TheDriver if l.x.id == driverID =>
                sendMess(cmd, driverID)
                false

              case l: APIDeviceDriver.DriverCommandDone if l.requestID == driverID =>
                done = true
                false

              case l: APIDeviceDriver.DriverStateChange if l.id == driverID =>
                done && 3 <= l.state.get("currentTime").flatMap(_.asOpt[Int]).getOrElse(-1)

          }
          }.getOrElse(false)

      }


    }


  }

  "SimulationTransportDriverTest" - {

    "running" in {
      val driverID = ID.newID
      val d = VD.Driver(
        "test",
        driverID,
        VolvoTransportSimulationDriver.driverType,
        SPAttributes(
          "length" -> 30,
          "sensors" -> List(Sensor("s1", 5, false), Sensor("s2", 25, false))
        )
      )

      val setup = APIDeviceDriver.SetUpDeviceDriver(d)
      sendMess(setup)

      val p = TestProbe()
      mediator ! Subscribe(APIDeviceDriver.topicResponse, p.ref)
      val cmd = APIDeviceDriver.DriverCommand(driverID,
        Map(
          "start"->true,
          "newBodyID" -> "b1",
          "newBodyLength" -> 10
        )
      )



      var done = false
      p.fishForMessage(10 second){
        case x: String =>
          val spmess = SPMessage.fromJson(x)
          println("transport: " + spmess)

          // The driver has started, send the command
          spmess.flatMap{ _.getBodyAs[APIDeviceDriver.Response].collect {
              case l: APIDeviceDriver.TheDriver  if l.x.id == driverID =>
                sendMess(cmd, driverID)
                false

              case l: APIDeviceDriver.DriverCommandDone if l.requestID == driverID =>
                done = true
                false

              case l: APIDeviceDriver.DriverStateChange if l.id == driverID =>
                done && l.state.get("s1").flatMap(_.asOpt[Boolean]).getOrElse(false)

            }
          }.getOrElse(false)

        case x => println(x); false


      }
    }


  }


  "SimulationPressureDriverTest" - {
    "running" in {
      val driverID = ID.newID
      val d = VD.Driver(
        "test",
        driverID,
        VolvoPressureSimulationDriver.driverType,
        SPAttributes()
      )

      val setup = APIDeviceDriver.SetUpDeviceDriver(d)
      sendMess(setup)

      val p = TestProbe()
      mediator ! Subscribe(APIDeviceDriver.topicResponse, p.ref)
      val cmd = APIDeviceDriver.DriverCommand(driverID,
        Map(
          "ref"->3
        )
      )



      var done = false
      p.fishForMessage(10 second){
        case x: String =>
          val spmess = SPMessage.fromJson(x)
          println(spmess)

          // The driver has started, send the command
          spmess.flatMap{ _.getBodyAs[APIDeviceDriver.Response].collect {
            case l: APIDeviceDriver.TheDriver  if l.x.id == driverID =>
              sendMess(cmd, driverID)
              false

            case l: APIDeviceDriver.DriverCommandDone if l.requestID == driverID =>
              done = true
              false

              case l: APIDeviceDriver.DriverStateChange if l.id == driverID =>
                val res = l.state.get("act").flatMap(_.asOpt[Int]).getOrElse(-1)
                val res2 = l.state.get("atRef").flatMap(_.asOpt[Boolean]).getOrElse(false)
                done && res == 3 && res2

            }
          }.getOrElse(false)

        case x => println(x); false


      }
    }


  }


  def sendMess(x: APIDeviceDriver.Request, reqID: ID = ID.newID) = {
    val toSend = SPMessage.makeJson(SPHeader(from = "testing", reqID = reqID), x)
    mediator ! Publish(APIDeviceDriver.topicRequest, toSend)
  }




}


