// package sp.models.sdu

// import org.scalatest._

// import akka.testkit._
// import akka.actor.ActorSystem
// import com.typesafe.config.ConfigFactory
// import akka.stream._
// import akka.stream.scaladsl._
// import akka.{ NotUsed, Done }
// import scala.concurrent.duration._
// import scala.concurrent._

// import sp.domain.Logic._
// import sp.domain._
// import sp.virtualdevice._
// import sp.virtualdevice.APISPVD._



// class ModelTest(_system: ActorSystem) extends TestKit(_system) with FreeSpecLike with Matchers with BeforeAndAfterAll {

//   def this() = this(ActorSystem("SP", ConfigFactory.parseString(
//     """
//       | akka.actor.provider = "cluster"
//       | akka.remote.netty.tcp.port=0
//       | akka.remote.artery.canonical.port=0
//     """.stripMargin)))

//   override def afterAll {
//     TestKit.shutdownActorSystem(system)
//   }

//   val model = new sp.sdu.Model(system)
//   val operations = model.operations
//   val idables = model.getIDAbles()
//   val init = model.getInitialState()
//   val resources = model.makeResources()

//   operations.foreach { o=>println(o.id + " - " + o.name) }

//   idables.foreach { println }

//   println("Initial state")
//   init.map { case (id, value) =>
//     println(idables.find(_.id == id).get.name + " - " + value.toString)
//   }

//   println("Resources")
//   resources.foreach { println }

//   // assert(false)

//   // start runner pipeline....

//   val initStateWithResources = init ++
//   resources.foldLeft(State.empty){case (s,r) => s++r.initialState}

//   val id = ID.newID

//   val runner = sp.runners.RunnerPipeline(
//     operations = operations,
//     transitionSystem = AbilityRunnerTransitions.abilityTransitionSystem,
//     initialState = SPState("initial state", initStateWithResources),
//     name = "test runner",
//     system = system
//   )

//   runner.makeTransitionsUnControlled(List(AbilityRunnerTransitions.AbilityTransitions.enabledToStarting.id))
//   runner.makeTransitionsUnControlled(List(AbilityRunnerTransitions.AbilityTransitions.finToNotEnabled.id))

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
// }
