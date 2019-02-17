package sp.runners

import akka.NotUsed
import akka.actor._
import sp.domain._
import Logic._
import akka.stream._
import akka.stream.scaladsl._

import Shared._
import sp.streams.SPStreamSupport._

object RunnerManager {
  def props = Props(classOf[RunnerManager])
}

class RunnerManager extends Actor
    with ActorLogging
    with sp.service.ServiceCommunicationSupport
    with sp.service.MessageBussSupport {

  subscribe(APIRunnerManager.topicRequest)

  var runners: Map[ID, ActorRef] = Map()

  override def receive = {
    //case x if {log.debug(s"Virtual device maker got: $x"); false} => false

    case setup: API.SetupRunnerInstance =>
      log.debug("Setting up VD")
      log.debug(setup.toString)
      if (runners.contains(setup.id)){
        log.debug("Runner already exists")
        // publish(APIRunnerManager.topicResponse, SPMessage.makeJson(updH, APISP.SPError(s"VD with id ${setup.id} already exist")))
      } else {
        val a = context.actorOf (RunnerInstance.props (setup) )
        runners += setup.id -> a
        context.watch (a)

        val header = SPHeader(from = APIRunnerManager.service)
        publish(APIRunnerManager.topicResponse, SPMessage.makeJson(header, APIRunnerManager.RunnerStarted(setup.id)))
      }


    case x: String =>
      val mess = SPMessage.fromJson(x)
      for {
        m <- mess
        h <- m.getHeaderAs[SPHeader] // add filter here if we want multiple makers
        b <- m.getBodyAs[APIRunnerManager.Request]
      } yield {
        val updH = h.swapToAndFrom()
        b match {

          case APIRunnerManager.TerminateRunnerInstance(id) =>
            runners.get(id).foreach(_ ! PoisonPill)
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPDone () ) )

          case APIRunnerManager.TerminateAllRunnerInstances =>
            println("Terminating runners!")
            runners.foreach(_._2 ! PoisonPill)
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPDone () ) )
          case x =>
        }

      }

    case Terminated(x) =>
      println("runner terminated!")
      runners = runners.filterNot(_._2 == x) // remove VD from VDs map, send message that VD was terminated, if there are no more VDs: send that all have been terminated
      runners.find(_._2 == x).foreach(vd => publish (APIRunnerManager.topicResponse, SPMessage.makeJson (SPHeader(from = APIRunnerManager.service), APIRunnerManager.TerminatedRunnerInstance(vd._1) )))
      if(runners.isEmpty) publish (APIRunnerManager.topicResponse, SPMessage.makeJson (SPHeader(from = APIRunnerManager.service), APIRunnerManager.TerminatedAllRunnerInstances ) )
  }


}


import akka.stream._
import akka.stream.scaladsl._
import scala.concurrent.duration._
import scala.concurrent.Future


object RunnerInstance {
  def props(setup: API.SetupRunnerInstance) = Props(classOf[RunnerInstance], setup)
}

class RunnerInstance(setup: API.SetupRunnerInstance) extends Actor
    with ActorLogging
    with sp.service.ServiceCommunicationSupport
    with sp.service.MessageBussSupport {

  var forceTable: Map[ID, SPValue] = Map()
  var forceEvents: List[sp.runners.RunnerLogic.FireEvent] = List()

  import context.dispatcher
  implicit val materializer = ActorMaterializer() // maybe send this in another way...

  val id = setup.id

  subscribe(APIRunnerManager.topicRequest)

  val runner = sp.runners.RunnerPipeline(
    operations = setup.runner.operations,
    transitionSystem = setup.runner.transitionSystem,
    initialState = SPState("initial state", setup.runner.initialState),
    name = setup.id.toString,
    system = context.system
  )

  // starting in "pause" mode with automatic reset
  // TODO: these id:s should come from outside...
  runner.makeTransitionsControlled(List(AbilityRunnerTransitions.AbilityTransitions.enabledToStarting.id))
//  runner.makeTransitionsUnControlled(List(AbilityRunnerTransitions.AbilityTransitions.finToNotEnabled.id))

  val resourceSources = mergeSources(setup.resources.map(r=>r.inputs).flatten)
  val resourceSinks = mergeSinks(setup.resources.map(r=>r.outputs).flatten)

  // twice per second, let frontend know the state
  val limitFrontend = normalizeRate[State](500 millis)
  val frontendSink = Sink.foreach[State] { s =>
    val header = SPHeader(from = id.toString)
    val body = APIRunnerManager.StateEvent(id, s)
    val message = SPMessage.makeJson(header, body)
    publish(APIRunnerManager.topicResponse, message)
  }
  val frontendFlow = limitFrontend.to(frontendSink)

  // keep-alive source
  val ticker = Source.tick(500 millis, 500 millis, Map())

  val killSwitch = resourceSources.merge(ticker)
    .map(state => state ++ forceTable) // force inputs and internal
    .map(state => sp.runners.StateUpd(SPState("test", state), forceEvents)) // force events
    .via(runner.runnerFlow)
    .map(_.state)
    .map(state => state ++ forceTable) // force outputs
    .viaMat(KillSwitches.single)(Keep.right)
    .alsoTo(frontendFlow)
    .to(resourceSinks)
    .run()

  override def receive = {
    // todo: handle some commands like pausing the runner
    case x: String =>
      val mess = SPMessage.fromJson(x)
      for {
        m <- mess
        h <- m.getHeaderAs[SPHeader] // add filter here if we want multiple makers
        b <- m.getBodyAs[APIRunnerManager.Request]
      } yield {
        val updH = h.swapToAndFrom()
        b match {

          case APIRunnerManager.SetPlan(instanceID, plan) if instanceID == id =>
            println("Setting new plan: " + plan.mkString(","))
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPACK () ) )
            runner.setPlan(plan)
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPDone () ) )


          case APIRunnerManager.StopAuto(instanceID) if instanceID == id =>
            println("Stopping auto")
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPACK () ) )
            runner.makeTransitionsControlled(List(AbilityRunnerTransitions.AbilityTransitions.enabledToStarting.id))
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPDone () ) )

          case APIRunnerManager.StartAuto(instanceID) if instanceID == id =>
            println("Starting auto")
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPACK () ) )
            runner.makeTransitionsUnControlled(List(AbilityRunnerTransitions.AbilityTransitions.enabledToStarting.id))
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPDone () ) )

          case APIRunnerManager.SetForceTable(instanceID, force, events) if instanceID == id =>
            println("Setting force table: " + force.mkString("\n"))
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPACK () ) )
            forceTable = force
            forceEvents = events.map{ case (id, spval) => sp.runners.RunnerLogic.FireEvent(spval, id) }.toList
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPDone () ) )

          case APIRunnerManager.TerminateRunnerInstance(instanceID) if instanceID == id =>
            // publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPACK () ) )
            // killSwitch.shutdown()
            // publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPDone () ) )

          case APIRunnerManager.TerminateAllRunnerInstances =>
            // publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPACK () ) )
            // killSwitch.shutdown()
            // publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPDone () ) )


          case _ =>

        }
      }

    case _ =>
  }
}
