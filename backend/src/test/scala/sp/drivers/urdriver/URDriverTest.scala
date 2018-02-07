package sp.drivers.urdriver

import akka.actor._
import akka.cluster.pubsub._
import akka.testkit._
import com.typesafe.config._
import org.scalatest._
import sp.abilityhandler.{APIAbilityHandler => api}
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
    """.stripMargin)))

  //val mediator = DistributedPubSub(system).mediator
  val id = ID.newID

  val v1 = Thing("v1")
  val pre = Condition(EQ(v1.id, 1), List(Action(v1.id, ValueHolder(2))))
  val post = Condition(EQ(v1.id, 3), List(Action(v1.id, ValueHolder(4))))
  val started = Condition(EQ(v1.id, 2), List())
  val reset = Condition(AlwaysTrue, List(Action(v1.id, ValueHolder(1))))
  val ability = api.Ability("test", id, pre, started, post, reset)


  override def beforeAll: Unit = {

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







}


