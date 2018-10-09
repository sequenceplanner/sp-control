package sp.virtualdevice

import akka.NotUsed
import akka.actor._
import sp.devicehandler._
import sp.domain._
import Logic._
import akka.stream._
import akka.stream.scaladsl._

// The VD maker
// makes a VD and then sends the ability to it when we get the make ability handler
// this should be merged later KB:180928
object SPVirtualDeviceMaker {
  def props = Props(classOf[SPVirtualDeviceMaker])
}

object SPVirtualDeviceInfo {
  import sp.domain.SchemaLogic._

  case class VirtualDeviceRequest(request: APIVirtualDevice.Request)
  case class VirtualDeviceResponse(response: APIVirtualDevice.Response)

  val req: com.sksamuel.avro4s.SchemaFor[VirtualDeviceRequest] = com.sksamuel.avro4s.SchemaFor[VirtualDeviceRequest]
  val resp: com.sksamuel.avro4s.SchemaFor[VirtualDeviceResponse] = com.sksamuel.avro4s.SchemaFor[VirtualDeviceResponse]

  val apischema = makeMeASchema(
    req(),
    resp()
  )

  private val id = ID.newID
  val attributes: APISP.StatusResponse = APISP.StatusResponse(
    service = "NewVirtualDeviceMaker",
    instanceID = Some(id),
    instanceName = s"VD-$id",
    tags = List("virtual device", "vd", "runtime", "communication"),
    api = apischema,
    version = 1,
    attributes = SPAttributes.empty
  )
}


class SPVirtualDeviceMaker extends Actor
  with ActorLogging
  with sp.service.ServiceCommunicationSupport
  with sp.service.MessageBussSupport {

  // Reusing the old info
  triggerServiceRequestComm(SPVirtualDeviceInfo.attributes)

  subscribe(APIVirtualDevice.topicRequest)

  var vds: Map[ID, ActorRef] = Map()

  override def receive = {
    //case x if {log.debug(s"Virtual device maker got: $x"); false} => false
    case x: String =>
      val mess = SPMessage.fromJson(x)
      for {
        m <- mess
        h <- m.getHeaderAs[SPHeader] // add filter here if we want multiple makers
        b <- m.getBodyAs[APIVirtualDevice.Request]
      } yield {
        val updH = h.swapToAndFrom()
        b match {

          case setup : APIVirtualDevice.SetUpVD2 =>
            log.debug("Setting up VD")
            log.debug(setup.toString)
            if (vds.contains(setup.id)){
              publish(APIVirtualDevice.topicResponse, SPMessage.makeJson(updH, APISP.SPError(s"VD with id ${setup.id} already exist")))
            } else {
              val a = context.actorOf (VirtualDevice.props (setup) )
              vds += setup.id -> a
              context.watch (a)
              publish (APIVirtualDevice.topicResponse, SPMessage.makeJson (updH, APISP.SPDone () ) )
            }

          case APIVirtualDevice.TerminateVD(id) =>
            vds.get(id).foreach(_ ! PoisonPill)
            publish (APIVirtualDevice.topicResponse, SPMessage.makeJson (updH, APISP.SPDone () ) )

          case APIVirtualDevice.TerminateAllVDs =>
            vds.foreach(_._2 ! PoisonPill)
            publish (APIVirtualDevice.topicResponse, SPMessage.makeJson (updH, APISP.SPDone () ) )
          case x =>
        }

      }

    case Terminated(x) => vds = vds.filterNot(_._2 == x) // remove VD from VDs map, send message that VD was terminated, if there are no more VDs: send that all have been terminated
      vds.find(_._2 == x).foreach(vd => publish (APIVirtualDevice.topicResponse, SPMessage.makeJson (SPHeader(from = APIVirtualDevice.service), APIVirtualDevice.TerminatedVD(vd._1) )))
      if(vds.isEmpty) publish (APIVirtualDevice.topicResponse, SPMessage.makeJson (SPHeader(from = APIVirtualDevice.service), APIVirtualDevice.TerminatedAllVDs ) )
  }


}


import akka.stream._
import akka.stream.scaladsl._
import scala.concurrent.duration._
import scala.concurrent.Future


object VirtualDevice {
  def props(setup: APIVirtualDevice.SetUpVD2) = Props(classOf[VirtualDevice], setup)
}

class VirtualDevice(setup: APIVirtualDevice.SetUpVD2) extends Actor with AbilityRunnerTransitions
  with ActorLogging
  with sp.service.ServiceCommunicationSupport
  with sp.service.MessageBussSupport {

  import context.dispatcher
  implicit val materializer = ActorMaterializer() // maybe send this in another way...

  val name = setup.name
  val id = setup.id

  subscribe(APIVirtualDevice.topicRequest)
  subscribe(APIDeviceDriver.topicResponse)

  // create driver -> resource sources from the old models
  def resourceReadStream(resource: VD.Resource) = {
    val mappers = resource.stateMap.collect { case VD.OneToOneMapper(id, did, driverIdent) => (id,did,driverIdent) }

    mappers.groupBy(_._2).map { case (did, mapper) =>
      val mapperMap = mapper.map(m => m._3 -> m._1).toMap
      val flow = Flow[Map[String, SPValue]].map(state => {
        val res = state.flatMap{ case (driverIdent, spval) => mapperMap.get(driverIdent).map(id => id -> spval) }.toMap
        // println(s"got input on ${did}: ${state} \n\nRESULTING IN $res")
        res
      })
      did -> flow
    }.toMap
  }

  def resourceWriteStream(resource: VD.Resource) = {
    val mappers = resource.stateMap.collect { case VD.OneToOneMapper(id, did, driverIdent) => (id,did,driverIdent) }

    mappers.groupBy(_._2).map { case (did, mapper) =>
      val mapperMap = mapper.map(m => m._1 -> m._3).toMap
      val flow = Flow[Map[ID, SPValue]].map(state => {
        state.flatMap{ case (id, spval) => mapperMap.get(id).map(did => did -> spval) }.toMap
      })
      did -> flow
    }.toMap
  }

  val nameMap = setup.model.map(idable => idable.id -> idable.name).toMap

  val dummyInit = setup.resources.flatMap(r => r.things.map(_ -> SPValue("noValue"))).toMap // here we need to change teh def of resources so we get things from the model

  val opInit = setup.operations.map(o => o.id -> SPValue("notEnabled")).toMap

  val initState = SPState("runnerState", dummyInit++setup.initialState++opInit) // här får vi skapa ett bättre init state där vi också inkl operations när de kommer

  val h = SPHeader(from = id.toString)
  setup.drivers.foreach{d =>
    publish(APIDeviceDriver.topicRequest, SPMessage.makeJson(h, APIDeviceDriver.SetUpDeviceDriver(d)))
  }
  val rreads = setup.resources.map(resourceReadStream).foldLeft(List[(ID, Flow[Map[String,SPValue],Map[ID,SPValue],NotUsed])]()){ case (acum, m) => acum ++ m.toList}.groupBy(_._1).map { case (k,v) => k -> v.map(_._2) }
  val rwrites = setup.resources.map(resourceWriteStream).foldLeft(List[(ID, Flow[Map[ID,SPValue],Map[String,SPValue],NotUsed])]()){ case (acum, m) => acum ++ m.toList}.groupBy(_._1).map { case (k,v) => k -> v.map(_._2) }


  val runner = sp.runners.RunnerPipeline(
    operations = setup.operations, // vi får tyvärr inte dessa här utan de kommer via abilities. Det kanske inte fungerar om abilitymaker komemr före VDmaker. Men vi testar såhär. Annars får vi uppdatera där vi går från modellen till dessa setup messages
    transitionSystem = abilityTransitionSystem, // def i AbilityRunnerTransitions trait nedan
    initialState = initState,
    name = setup.name,
    system = context.system
  )

  // starting in "pause" mode with automatic reset
  // runner.makeTransitionsControlled(List(AbilityTransitions.enabledToStarting.id)) // actually already set
  // runner.makeTransitionsUnControlled(List(AbilityTransitions.finToNotEnabled.id))

  // to auto run, call:
  runner.makeTransitionsUnControlled(List(AbilityTransitions.enabledToStarting.id))
  runner.makeTransitionsUnControlled(List(AbilityTransitions.finToNotEnabled.id))

  type resourceState = Map[ID, SPValue]
  type driverState = Map[String, SPValue]

  // temp sink to work with old stuff.......
  def driverSink(driverID: ID) = Sink.foreach[driverState]{ state =>
    println(s"driver sink for $driverID with $state")
    val command = APIDeviceDriver.DriverCommand(driverID, state)
    val header = SPHeader(from = id.toString)
    publish("driverCommands", SPMessage.makeJson(header, command))
  }

  val driverSinks = setup.drivers.map(d => d.id -> driverSink(d.id)).toMap
  val resourceSinks = rwrites.flatMap { case (did, flows) =>
    driverSinks.get(did).map(ds => flows.map(f => f.to(ds)))
  }.flatten
  val allSinks = resourceSinks.toList match {
    case Nil => Sink.ignore
    case first :: Nil => first
    case first :: second :: Nil => Sink.combine(first, second)(Broadcast[resourceState](_))
    case first :: second :: rest =>
      Sink.combine(first, second, rest:_*)(Broadcast[resourceState](_))
  }

  val driverSources = setup.drivers.map {d =>
    val source = Source.queue[driverState](100, akka.stream.OverflowStrategy.dropHead)
    val q = source.mapMaterializedValue { queue =>
      class DriverX extends Actor with sp.service.MessageBussSupport {
        subscribe(APIDeviceDriver.topicResponse)

        override def receive = {
          case x: String =>
            val mess = SPMessage.fromJson(x)
            for {
              m <- mess
              h <- m.getHeaderAs[SPHeader]
              b <- m.getBodyAs[APIDeviceDriver.Response]
            } yield {
              b match {
                case e @ APIDeviceDriver.DriverStateChange(_, did, state, _) if did == d.id =>
                  println(s"OFFERING DRIVER $did STATE $state")
                  queue.offer(state)
                case _ =>
              }
            }
          case _ =>
        }
      }
      context.actorOf(Props(new DriverX), d.id.toString)
    }
    d.id -> q
  }.toMap

  val resourceSources = rreads.flatMap { case (did, flows) =>
    driverSources.get(did).map(ds => {
      flows.map(f => ds.via(f))})
  }.flatten


  val allSources = resourceSources.toList match {
    case Nil => Source.empty[resourceState]
    case first :: Nil => first
    case first :: second :: Nil => Source.combine(first, second)(Merge[resourceState](_))
    case first :: second :: rest =>
      Source.combine(first, second, rest:_*)(Merge[resourceState](_))
  }

  val printSink = Sink.foreach[resourceState] { state =>
    println("NEW RUNNER STATE")
    state.map { case (id, v) =>
      val n = nameMap.get(id).getOrElse(id.toString)
      println(s"$n: $v")
    }
  }

  val printSource = Flow[resourceState].map { state =>
    println("NEW INPUT STATE")
    state.map { case (id, v) =>
      val n = nameMap.get(id).getOrElse(id.toString)
      println(s"$n: $v")
    }
    state
  }

  // add StateUpd to que and plug in flows and a sink to send SPState where you want
  allSources
    .via(printSource)
    .map(state => sp.runners.StateUpd(SPState("test", state), List()))

  .via(runner.runnerFlow(Some(2500 milliseconds))) // den tickar...
    .map(_.state)
    .merge(Source.tick(500.millis, 500.millis, Map()))
    .scan[resourceState](Map()){case (acum,state) => acum++state}
    .alsoTo(printSink)
    .to(allSinks)
    .run()

  override def receive = {

    case x: String =>
      val mess = SPMessage.fromJson(x)
      // receive driver data
      for {
        m <- mess
        h <- m.getHeaderAs[SPHeader]
        b <- m.getBodyAs[APIDeviceDriver.Response]
      } yield {
        //log.debug("VD from driver: " +b)
        b match {

          // case e @ APIDeviceDriver.DriverStateChange(_, did, state, _) =>

          //   dxs2.offer(state)

          //   driverSources.get(did).foreach { d =>

          //     d.mapMaterializedValue { q =>
          //       println(s"offering state: $state to $did")
          //       q.offer(state)
          //     }
          //   }

          case _ =>
        }
      }
      // receive other stuff from upper systems when needed

      // receive commands to add or remove ops. events, etc.

    case _ =>
  }



}


// Inte testad! Måste skriva test för denna så att det fungerar som tänkt
trait AbilityRunnerTransitions {
  import sp.runners.RunnerLogic._

  // states
  object AbilityStates {
    val notEnabled = "notEnabled"
    val enabled = "enabled"
    val starting = "starting"
    val executing = "executing"
    val finished = "finished"
  }

  // kinds
  object AbilityKinds {
    val pre = "pre"
    val started = "started"
    val post = "post"
    val postAlternative = "postAlternative"
    val reset = "reset"
  }

  object AbilityTransitions {
    import AbilityStates._
    import AbilityKinds._

    val notEnabledToEnabled = OperationTransition(
      states = Set(notEnabled),
      conditionKind = Set(pre),
      nextState = enabled,
      event = None,
      alwaysTrueIfNoConditions = true,
      enableAlternatives = false,
      onlyGuard = true,
      negateGuard = false
    )
    val enabledToNotEnabled = OperationTransition(
      states = Set(enabled),
      conditionKind = Set(pre),
      nextState = notEnabled,
      event = None,
      alwaysTrueIfNoConditions = false,
      enableAlternatives = false,
      onlyGuard = true,
      negateGuard = true // this will go back if pre guard is false when in enabled
    )
    val enabledToStarting = OperationTransition(
      states = Set(enabled),
      conditionKind = Set(pre),
      nextState = starting,
      event = Some("start"),
      alwaysTrueIfNoConditions = false,
      enableAlternatives = false,
      onlyGuard = false,
      negateGuard = false
    )
    val startingToExec = OperationTransition(
      states = Set(starting),
      conditionKind = Set(started),
      nextState = executing,
      event = None,
      alwaysTrueIfNoConditions = true,
      enableAlternatives = false,
      onlyGuard = false,
      negateGuard = false
    )

    val execToFinished = OperationTransition(
      states = Set(executing),
      conditionKind = Set(post),
      nextState = finished,
      event = None,
      alwaysTrueIfNoConditions = true,
      enableAlternatives = false,
      onlyGuard = false,
      negateGuard = false
    )
    val execToFinishedAlt = OperationTransition(
      states = Set(executing),
      conditionKind = Set(postAlternative),
      nextState = finished,
      event = None,
      alwaysTrueIfNoConditions = false,
      enableAlternatives = true,
      onlyGuard = false,
      negateGuard = false
    )
    val finToNotEnabled = OperationTransition(
      states = Set(finished),
      conditionKind = Set(reset),
      nextState = notEnabled,
      event = Some("reset"),
      alwaysTrueIfNoConditions = false,
      enableAlternatives = false,
      onlyGuard = false,
      negateGuard = false
    )
    val forceReset = OperationTransition(
      states = Set(starting, executing, finished),
      conditionKind = Set("ShouldNotHaveAnyConditions"),
      nextState = notEnabled,
      event = Some("forceReset"),
      alwaysTrueIfNoConditions = true,
      enableAlternatives = false,
      onlyGuard = false,
      negateGuard = false
    )
    val syncedExecution = OperationTransition( // For operations that should sync with reality
      states = Set(notEnabled, enabled, starting, finished),
      conditionKind = Set("isExecuting"),
      nextState = executing,
      event = None,
      alwaysTrueIfNoConditions = false,
      enableAlternatives = false,
      onlyGuard = true,
      negateGuard = false
    )
    val syncedFinished = OperationTransition( // For operations that should sync with reality
      states = Set(notEnabled, enabled, starting, executing),
      conditionKind = Set("isFinished"),
      nextState = finished,
      event = None,
      alwaysTrueIfNoConditions = false,
      enableAlternatives = false,
      onlyGuard = true,
      negateGuard = false
    )

  }


  val abilityTransitionSystem = List(
    AbilityTransitions.notEnabledToEnabled,
    AbilityTransitions.enabledToNotEnabled,
    AbilityTransitions.enabledToStarting,
    AbilityTransitions.startingToExec,
    AbilityTransitions.execToFinished,
    AbilityTransitions.execToFinishedAlt,
    AbilityTransitions.finToNotEnabled,
    AbilityTransitions.forceReset,
    AbilityTransitions.syncedExecution,
    AbilityTransitions.syncedFinished
  )
}
