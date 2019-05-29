package sp.runners

import akka.actor._
import sp.domain._
import sp.domain.Logic._
import sp.runners.PTM_Models.{PTMOperation, PTMTransition, StatePredicate}
import sp.runners.Shared._
import sp.streams.SPStreamSupport._

object PTMRunnerManagerOldModels {
  def props = Props(classOf[PTMRunnerManagerOldModels])
}

class PTMRunnerManagerOldModels extends Actor
    with ActorLogging
    with sp.service.ServiceCommunicationSupport
    with sp.service.MessageBussSupport {

  subscribe(APIRunnerManager.topicRequest)

  var runners: Map[ID, ActorRef] = Map()

  override def receive = {
    //case x if {log.debug(s"Virtual device maker got: $x"); false} => false

    case setup: API.SetupRunnerInstance =>
      log.debug("Setting up PTM Runner VD with Old Model")
      log.debug(setup.toString)
      if (runners.contains(setup.id)){
        log.debug("Runner already exists")
        // publish(APIRunnerManager.topicResponse, SPMessage.makeJson(updH, APISP.SPError(s"VD with id ${setup.id} already exist")))
      } else {
        val a = context.actorOf (PTMRunnerInstanceOldModel.props (setup) )
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


object PTMRunnerInstanceOldModel {
  def props(setup: API.SetupRunnerInstance) = Props(classOf[PTMRunnerInstanceOldModel], setup)
}

class PTMRunnerInstanceOldModel(setup: API.SetupRunnerInstance) extends Actor
    with ActorLogging
    with sp.service.ServiceCommunicationSupport
    with sp.service.MessageBussSupport {

  implicit val materializer = ActorMaterializer() // maybe send this in another way...

  val id = setup.id

  subscribe(APIRunnerManager.topicRequest)

  def makePredAndTrans(o: Operation, kind: String) = {
    val name = o.name + "_" + kind
    val kinds = o.conditions.filter(c => hasStrAttr(c.attributes, "kind", kind))
    val pred = StatePredicate(name, AND(kinds.map(k => k.guard)))
    val predState = EQ(pred.id, ValueHolder(true))
    val trans = PTMTransition(Condition(predState, kinds.flatMap(_.action)), name)
    (pred, trans)
  }
  def makeEffects(o: Operation, pred: StatePredicate, kind: String) = {
    val kinds = o.conditions.filter(c => hasStrAttr(c.attributes, "kind", kind))
    val predState = EQ(pred.id, ValueHolder(true))
    PTMTransition(Condition(predState, kinds.flatMap(_.action)), o.name + "_" + kind)
  }
  def foldConditions(xs: List[Condition]): Condition = {
    val first = xs.headOption.getOrElse(Condition(AlwaysTrue))
    xs.tail.foldLeft(first){case (aggr, c) =>
      val g = AND(List(aggr.guard, c.guard))
      val a = aggr.action ++ c.action
      aggr.copy(guard = g, action = a)
    }
  }

  val abilities = setup.runner.operations.filter(o => !hasStrAttr(o.attributes, "isa", "operation")).map{a =>
    val pre = makePredAndTrans(a, "pre")
    val startEffect = makeEffects(a, pre._1, "startEffect")
    val isExecuting = makePredAndTrans(a, "isExecuting")
    val executingEffect = makeEffects(a, isExecuting._1,"executingEffect")
    val isFinished = makePredAndTrans(a, "isFinished")
//    val reset = makePredAndTrans(a, "reset")

    PTMOperation(
      predicates = List(pre._1, isExecuting._1, isFinished._1),
      controlled = List(pre._2),
      unControlled = List(isExecuting._2, isFinished._2),
      effects = List(startEffect, executingEffect),
      a
    )
  }
  val operations = setup.runner.operations.filter(o => o.attributes.getAs[String]("isa").contains("operation")).map{o =>
    val pre =  foldConditions(o.conditions.filter(c => hasStrAttr(c.attributes, "kind", "pre")))
    val post =  foldConditions(o.conditions.filter(c => hasStrAttr(c.attributes, "kind", "post")))
    PTM_Models.makePlanningOP(o.name, pre, post, Some(o))
  }

  val initOPState = operations.map(_._2.id -> SPValue("i")).toMap

  val runner = sp.runners.PTMRunnerPipeline(
    operations = operations.map(_._1),
    abilities = abilities,
    initialState = SPState("RunnerState_"+setup.id, setup.runner.initialState ++ initOPState),
    initialOperationTransitionQue = List(),
    model = setup.items ++ operations.map(_._2),
    name = "runner_"+ setup.id,
    system = context.system
  )


  val resourceSources = mergeSources(setup.resources.flatMap(_.inputs))
  val resourceSinks = mergeSinks(setup.resources.flatMap(_.outputs))

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
    .map(state => SPState("upd", state.toMap)) // force events
    .via(runner.runnerFlow)
    .map(_.state)
    .viaMat(KillSwitches.single)(Keep.right)
    .alsoTo(frontendFlow)
    .to(resourceSinks)
    .run()


  // import context.dispatcher
  // context.system.scheduler.scheduleOnce(5 seconds) {
  //   runner.setRunnerData(PTMRunnerSetState(pause = Some(false)))
  // }


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

            // KB: Not sure what this does. We should handle new operations probably
          case APIRunnerManager.SetPlan(instanceID, plan) if instanceID == id =>
            println("Setting new plan: " + plan.mkString(","))
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPACK () ) )
            //runner.setPlan(plan)
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPDone () ) )

          case APIRunnerManager.StartAuto(instanceID) if instanceID == id =>
            println("Starting auto")
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPACK () ) )
            runner.setRunnerData(PTMRunnerSetState(pause = Some(false)))
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPDone () ) )

          case APIRunnerManager.StopAuto(instanceID) if instanceID == id =>
            println("Stopping auto")
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPACK () ) )
            runner.setRunnerData(PTMRunnerSetState(pause = Some(true)))
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPDone () ) )

          case APIRunnerManager.TakeStep(instanceID) if instanceID == id =>
            println("Taking control step")
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPACK () ) )
            runner.setRunnerData(PTMRunnerSetState(step = Some(Some(true))))
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPDone () ) )

          case APIRunnerManager.SetStepping(step, instanceID) if instanceID == id =>
            val setStep = if(!step) None else Some(false)
            println(s"Setting stepping to $step")
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPACK () ) )
            runner.setRunnerData(PTMRunnerSetState(step = Some(setStep)))
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPDone () ) )

          case APIRunnerManager.SetForceTable(instanceID, force, events) if instanceID == id =>
            println("Setting force table: " + force.mkString("\n"))
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPACK () ) )
            runner.setRunnerData(PTMRunnerSetState(forceState = Some(force)))
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPDone () ) )

          case APIRunnerManager.SetForceGoal(instanceID, goal) if instanceID == id =>
            println("Setting force goal: " + goal)
            publish (APIRunnerManager.topicResponse, SPMessage.makeJson (updH, APISP.SPACK () ) )
            runner.setRunnerData(PTMRunnerSetState(forceGoal = Some(goal)))
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

  def hasStrAttr(attr: SPAttributes, key: String, value: String): Boolean = attr.getAs[String](key).contains(value)
}
