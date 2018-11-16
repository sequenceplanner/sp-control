package sp.unification

import scala.concurrent.duration._
import scala.concurrent._
import org.scalatest._
import sp.domain.Logic._
import sp.domain._
import sp.runners.APIOperationRunner
import akka.stream._
import akka.stream.scaladsl._
import akka.NotUsed
import akka.testkit._
import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import sp.virtualdevice._

class NewRunnerTest(_system: ActorSystem) extends TestKit(_system)
    with FreeSpecLike
    with Matchers
with BeforeAndAfterAll {

  def this() = this(ActorSystem("SP", ConfigFactory.parseString(
    """
      |
    """.stripMargin)))

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }


  println
  println
  val idables = TestDummy().buildModel()
  println
  println
  println
  println

  implicit val materializer = ActorMaterializer() // maybe send this in another way...

  val ops = idables.collect{case o: Operation => o}
  val things = idables.collect{case t: Thing if t.attributes.keys.contains("domain") => t}
  val thingMap = things.map(t=>t.name -> t.id).toMap

  val initState = SPState("state", ops.map(o => o.id -> SPValue(AbilityRunnerTransitions.AbilityStates.notEnabled)).toMap ++ things.map(t => t.id -> t.attributes.getAs[List[SPValue]]("domain").get.head).toMap)

  val runner = sp.runners.RunnerPipeline(
    operations = ops, // vi får tyvärr inte dessa här utan de kommer via abilities. Det kanske inte fungerar om abilitymaker komemr före VDmaker. Men vi testar såhär. Annars får vi uppdatera där vi går från modellen till dessa setup messages
    transitionSystem = AbilityRunnerTransitions.abilityTransitionSystem, // def i AbilityRunnerTransitions trait nedan
    initialState = initState,
    name = "runner",
    system = system
  )

  // add StateUpd to que and plug in flows and a sink to send SPState where you want
  val runnerPipelineSource =
    Source.queue[sp.runners.StateUpd](100, akka.stream.OverflowStrategy.backpressure)
    .via(runner.runnerFlow)


  // val t1 = SPState("state",Map(ID.makeID("d0cde7fc-70b6-443e-b712-27ec78fcce5a").get -> "notEnabled", ID.makeID("3521e042-acd4-4238-8192-cf1754040c79").get -> "notEnabled", ID.makeID("b7c3b0f1-0792-4ed4-9820-f59191ae5c7b").get -> "a", ID.makeID("67ea530e-5325-4ecc-9662-0b714854fa04").get -> true))
  // val t2 = List(Condition(AND(List(EQ(SVIDEval(ID.makeID("67ea530e-5325-4ecc-9662-0b714854fa04").get),ValueHolder(true)), EQ(SVIDEval(ID.makeID("b7c3b0f1-0792-4ed4-9820-f59191ae5c7b").get),ValueHolder("a")))),List(Action(ID.makeID("b7c3b0f1-0792-4ed4-9820-f59191ae5c7b").get,ValueHolder("b"))),SPAttributes("kind"->"pre")))
  // assert(t2.forall(p=>p.eval(t1)))

  // go from SPState to resources with state
  //val resourceFlow

  // go from resources and state to driver commands
  // val driverFlow

  // Also add a check if the state for the resource has not changed and only forward if new
  // after that, we can add a throttle


  val ((queue, ks), fut) = runnerPipelineSource
    .viaMat(KillSwitches.single)(Keep.both)
    .toMat(Sink.foreach{x=>println("XXXXXXX: " +x)})(Keep.both)
    .run()

  val spstate1 = SPState("upd", Map(thingMap("active") -> SPValue("yes")))
  val spstate2 = SPState("upd", Map(thingMap("active") -> SPValue("no")))

  import scala.concurrent.ExecutionContext.Implicits.global
  system.scheduler.scheduleOnce(200.millis)(queue.offer(sp.runners.StateUpd(spstate1, List())))
  system.scheduler.scheduleOnce(2500.millis)(queue.offer(sp.runners.StateUpd(spstate2, List())))
  system.scheduler.scheduleOnce(3500.millis)(ks.shutdown())

  val res = Await.result(fut, 4.seconds)
  println("wait done, exiting: " + res)

}
