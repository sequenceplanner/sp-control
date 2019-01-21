package sp.models.unification.ipsintegration

import org.scalatest._

import akka.testkit._
import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import akka.stream._
import akka.stream.scaladsl._
import akka.{ NotUsed, Done }
import scala.concurrent.duration._
import scala.concurrent._

import sp.domain.Logic._
import sp.domain._

import sp.streams._

import sp.runners._
import sp.runners.Shared._



class IPSIntegrationTest(_system: ActorSystem) extends TestKit(_system) with FreeSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("SP", ConfigFactory.parseString(
    """
      | akka.actor.provider = "cluster"
      | akka.remote.netty.tcp.port=0
      | akka.remote.artery.canonical.port=0
    """.stripMargin)))

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }


  println("\n\n\n")
  println(System.getProperty("java.class.path"))
  println("\n\n\n")

  println("\n\n\n")
  println(System.getProperty("java.library.path"))
  println("\n\n\n")


  import sp.drivers.ros2.ROSHelpers
  val nestedMsg = ROSHelpers.createROSMsg("geometry_msgs/Pose").get
  val nestedAttr = ROSHelpers.msgToAttr(nestedMsg)
  val newPosition = SPAttributes("position" -> Map ("x" -> 5))
  val updatedAttr = nestedAttr.merge(newPosition)
  println(updatedAttr)
  val chantedNested = ROSHelpers.attrToMsg(updatedAttr)
  println("SP CREATED A MESSAGE:")
  val changedAttr = ROSHelpers.msgToAttr(chantedNested)
  println(changedAttr)

  /// now we add support for lists/arrays

  println("\n\n\n")

  import sp.drivers.ros2.ROSHelpers
  val listMsg = ROSHelpers.createROSMsg("sensor_msgs/MultiDOFJointState").get
  val listAttr = ROSHelpers.msgToAttr(listMsg)
  println("original message: " + listAttr)
  val jointNames = SPAttributes("joint_names" -> List("hej", "hopp"))
  val twist = SPAttributes("twist" -> List(SPAttributes("linear" -> SPAttributes("x" -> 5))))
  val newListAttr = listAttr.merge(jointNames).merge(twist)
  val newListMsg = ROSHelpers.attrToMsg(newListAttr)
  println("merged with sp stuff: " + newListAttr)
  val convertBack = ROSHelpers.msgToAttr(newListMsg)
  println("converted back to ros message: " + convertBack)

//  assert(false)



  import sp.drivers.ros2.ROSHelpers
  val uni2spmsg = ROSHelpers.createROSMsg("unification_ros2_messages/URPoseUniToSP").get
  val sp2unimsg = ROSHelpers.createROSMsg("unification_ros2_messages/URPoseSPToUni").get

  val uni2spAttr = ROSHelpers.msgToAttr(uni2spmsg)
  val sp2uniAttr = ROSHelpers.msgToAttr(sp2unimsg)

  println(uni2spAttr)
  println(sp2uniAttr)

//  assert(false)

  val t0 = System.nanoTime()
  val model = new IPSIntegrationModel(system)
  val t1 = System.nanoTime()
  println("Time to make model: " + (t1 - t0) / 1e9d + " seconds")

  val operations = model.operations
  val idables = model.getIDAbles()
  val init = model.getInitialState()
  val resources = model.makeResources()

  idables.foreach { println }

  println("Initial state")
  init.map { case (id, value) =>
    println(idables.find(_.id == id).get.name + " - " + value.toString)
  }
  assert(false)

  println("Resources")
  resources.foreach { println }

  // start runner pipeline....

  val initStateWithResources = init ++
    resources.foldLeft(State.empty){case (s,r) => s++r.initialState}

  val id = ID.newID

  val runner = sp.runners.RunnerPipeline(
    operations = operations,
    transitionSystem = AbilityRunnerTransitions.abilityTransitionSystem,
    initialState = SPState("initial state", initStateWithResources),
    name = "test runner",
    system = system
  )

  runner.makeTransitionsUnControlled(List(AbilityRunnerTransitions.AbilityTransitions.enabledToStarting.id))
  runner.makeTransitionsUnControlled(List(AbilityRunnerTransitions.AbilityTransitions.finToNotEnabled.id))

  val resourceSources = SPStreamSupport.mergeSources(resources.map(r=>r.inputs).flatten)
  val resourceSinks = SPStreamSupport.mergeSinks(resources.map(r=>r.outputs).flatten)

  // add StateUpd to que and plug in flows and a sink to send SPState where you want
  val ks = resourceSources
    .map(state => sp.runners.StateUpd(SPState("test", state), List()))
    .via(runner.runnerFlow)
    .map(_.state).map{s =>
      s.foreach { case (id, value) =>
        println(idables.find(_.id == id).get.name + " - " + value.toString)
      }
      s
    }
    .viaMat(KillSwitches.single)(Keep.right)
    .to(resourceSinks)
    .run()(ActorMaterializer())

  val dieAfter = 30

  import scala.concurrent.ExecutionContext.Implicits.global
  system.scheduler.scheduleOnce(dieAfter.seconds)(ks.shutdown())

  Thread.sleep(dieAfter*1000)
}
