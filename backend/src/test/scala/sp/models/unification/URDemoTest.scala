package sp.models.unification.urdemo2

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



class URDemoTest(_system: ActorSystem) extends TestKit(_system) with FreeSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("SP", ConfigFactory.parseString(
    """
      | akka.actor.provider = "cluster"
      | akka.remote.netty.tcp.port=0
      | akka.remote.artery.canonical.port=0
    """.stripMargin)))

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }


  import sp.drivers.ros2.ROSHelpers
  val uni2spmsg = ROSHelpers.createROSMsg("unification_ros2_messages/MoveItUniToSP").get
  val sp2unimsg = ROSHelpers.createROSMsg("unification_ros2_messages/MoveItSPToUni").get

  val robotInput = ROSHelpers.createROSMsg("unification_ros2_messages/RobotiqUniToSP").get
  val robotOutput = ROSHelpers.createROSMsg("unification_ros2_messages/RobotiqSPToUni").get

  val uni2spAttr = ROSHelpers.msgToAttr(uni2spmsg)
  val sp2uniAttr = ROSHelpers.msgToAttr(sp2unimsg)

  val robotInputAttr = ROSHelpers.msgToAttr(robotInput)
  val robotOutputAttr = ROSHelpers.msgToAttr(robotOutput)

  println(uni2spAttr)
  println(sp2uniAttr)
  println(robotInputAttr)
  println(robotOutputAttr)

//  assert(false)

  val t0 = System.nanoTime()
  val model = new Demo(system)
  val t1 = System.nanoTime()
  println("Time to make model: " + (t1 - t0) / 1e9d + " seconds")

  val operations = model.operations
  val idables = model.getIDAbles()
  val init = model.getInitialState()
  val resources = model.makeResources()

  operations.foreach { o=>println(o.id + " - " + o.name) }
  idables.find(_.name == "human.takeBlue").foreach(x=>println(x.asInstanceOf[Operation].conditions))

  assert(false)

  idables.foreach { println }

  println("Initial state")
  init.map { case (id, value) =>
    println(idables.find(_.id == id).get.name + " - " + value.toString)
  }

  println("Resources")
  resources.foreach { println }

  // start runner pipeline....

  val initStateWithResources = init ++
    resources.foldLeft(State.empty){case (s,r) => s++r.initialState}

  val id = ID.newID

//   val runner = sp.runners.RunnerPipeline(
//     operations = operations,
//     transitionSystem = AbilityRunnerTransitions.abilityTransitionSystem,
//     initialState = SPState("initial state", initStateWithResources),
//     name = "test runner",
//     system = system
//   )

//   runner.makeTransitionsUnControlled(List(AbilityRunnerTransitions.AbilityTransitions.enabledToStarting.id))
// //  runner.makeTransitionsUnControlled(List(AbilityRunnerTransitions.AbilityTransitions.finToNotEnabled.id))

//   val resourceSources = SPStreamSupport.mergeSources(resources.map(r=>r.inputs).flatten)
//   val resourceSinks = SPStreamSupport.mergeSinks(resources.map(r=>r.outputs).flatten)

//   // add StateUpd to que and plug in flows and a sink to send SPState where you want
//   val ks = resourceSources
//     .map(state => sp.runners.StateUpd(SPState("test", state), List()))
//     .via(runner.runnerFlow)
//     .map(_.state).map{s =>
//       s.foreach { case (id, value) =>
//         println(idables.find(_.id == id).get.name + " - " + value.toString)
//       }
//       s
//     }
//     .viaMat(KillSwitches.single)(Keep.right)
//     .to(resourceSinks)
//     .run()(ActorMaterializer())

//   val dieAfter = 30

//   import scala.concurrent.ExecutionContext.Implicits.global
//   system.scheduler.scheduleOnce(dieAfter.seconds)(ks.shutdown())

//   Thread.sleep(dieAfter*1000)
}
