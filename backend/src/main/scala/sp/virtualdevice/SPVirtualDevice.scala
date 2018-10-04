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
  with OldVirtualDeviceLogic
  with sp.service.ServiceCommunicationSupport
  with sp.service.MessageBussSupport {

  import context.dispatcher
  implicit val materializer = ActorMaterializer() // maybe send this in another way...

  val name = setup.name
  val id = setup.id

  subscribe(APIVirtualDevice.topicRequest)
  subscribe(APIDeviceDriver.topicResponse)


  def makeVDMessage = {
    APIVirtualDevice.TheVD(
      name = setup.name,
      id = setup.id,
      resources = setup.resources.map(r => VD.ResourceWithState(r, resourceState.getOrElse(r.id, Map[ID, SPValue]()))),
      drivers = setup.drivers.map(d => VD.DriverWithState(d, driverState.getOrElse(d.id, Map[String, SPValue]()))),
      attributes = setup.attributes
    )
  }


  // set up the VD with the data. Should include ops and the message structure in some way. Or we try with one two one mapping
  // Setup


  val dummyInit = setup.resources.flatMap(r => r.things.map(_ -> SPValue("noValue"))).toMap // here we need to change teh def of resources so we get things from the model

  val opInit = setup.operations.map(o => o.id -> SPValue("notEnabled")).toMap

  val initState = SPState("runnerState", setup.initialState++dummyInit++opInit) // här får vi skapa ett bättre init state där vi också inkl operations när de kommer

  val h = SPHeader(from = id.toString)
  setup.drivers.foreach{d =>
    newDriver(d)
    publish(APIDeviceDriver.topicRequest, SPMessage.makeJson(h, APIDeviceDriver.SetUpDeviceDriver(d)))
  }
  setup.resources.foreach(newResource)
  publish(APIVirtualDevice.topicRequest, SPMessage.makeJson(h, makeVDMessage))




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

  // add StateUpd to que and plug in flows and a sink to send SPState where you want
  val runnerPipelineSource =
    Source.queue[sp.runners.StateUpd](100, akka.stream.OverflowStrategy.backpressure)
    .via(runner.runnerFlow(Some(2500 milliseconds))) // den tickar...

  // temp sink to work with old stuff.......
  val resourceSink = Sink.foreach[SPState]{ state =>
    val s = state.state
    val rids = resources.values.map(rws => rws.r.id -> rws.r.things).toMap
    val res = rids.filter(_._2.intersect(s.keySet).nonEmpty)

    res.foreach { case (rid, rthings) =>
      val body = APIVirtualDevice.VDCommand(rid, s.filter(kv => rthings.contains(kv._1)))
      val diffs = getDriverDiffs(body)

      if(diffs.isEmpty || diffs.forall { case (k,v) => v.isEmpty }) {
        println("No driver variables to update.")
      } else {
        val commands = getDriverCommands(diffs)
        // send commands to the drivers
        commands.foreach { command =>
          val header = SPHeader(from = id.toString)
          publish("driverCommands", SPMessage.makeJson(header, command))
        }
      }
    }
  }

  val queue = runnerPipelineSource.alsoTo(Sink.foreach(x=>println("runner: " + x)))
    .to(resourceSink)
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

          case e @ APIDeviceDriver.DriverStateChange(_, did, _, _) if drivers.contains(did) =>
            //println("GOT DRIVER EVENT")
            //println("got a statechange:" + e)
            val oldrs = resourceState
            driverEvent(e)
            //println("new driver state: " + driverState)
            //println("new resource state: " + resourceState)

            resourceState.filter { case (nid, ns) =>
              oldrs.get(nid) match {
                case Some(os) => (ns.toSet diff os.toSet).nonEmpty
                case None => true
              }
            }.foreach { case (rid, state) if resources.contains(rid) =>
                println("offering state: " + state)
                queue.offer(sp.runners.StateUpd(SPState("test", state), List()))
            }

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

trait OldVirtualDeviceLogic {
  val name: String
  val id: ID

  case class DriverCommandTimeout(requestID: ID, timeout: Int)

  case class StateReader(f: (Map[ID, VD.DriverState], Map[ID, VD.State]) =>  Map[ID, VD.State])
  case class StateWriter(f: (Map[ID, VD.State], Map[ID, VD.DriverState]) =>  Map[ID, VD.DriverState])

  case class ResourceWithState(r: VD.Resource, read: List[StateReader], write: List[StateWriter])

  var drivers: Map[ID, VD.Driver] = Map()
  var driverState: Map[ID, VD.DriverState] = Map()
  var activeDriverRequests: Map[ID, List[ID]] = Map()

  var resources: Map[ID, ResourceWithState] = Map()
  var resourceState: Map[ID, VD.State] = Map()

  def newDriver(d: VD.Driver) = {
    drivers += d.id -> d
    driverState += d.id -> Map[String, SPValue]()
  }

  def newResource(resource: VD.Resource) = {
    val rw = resource.stateMap.flatMap {
      case VD.OneToOneMapper(t, id, name) =>
        val reader = StateReader{ case (drivers, resources) =>
          val nr = for {
            driver <- drivers.get(id)
            value <- driver.get(name)
            rs <- resources.get(resource.id)
          } yield {
            resources + (resource.id -> (rs + (t -> value)))
          }
          nr.getOrElse(resources)
        }
        val writer = StateWriter{ case (resources, drivers) =>
          val nd = for {
            rs <- resources.get(resource.id)
            value <- rs.get(t)
            driver <- drivers.get(id)
          } yield {
            drivers + (id -> (driver + (name -> value)))
          }
          nd.getOrElse(drivers)
        }
        Some((reader,writer))
      // potentially add other mapping types here
    }

    resources += resource.id -> ResourceWithState(resource, rw.map(_._1), rw.map(_._2))
    resourceState += resource.id -> Map()
  }

  def driverEvent(e: APIDeviceDriver.DriverStateChange) = {
    val current = driverState.get(e.id)
    current.foreach{ state =>
      val upd = state ++ e.state
      driverState += e.id -> upd
    }
    resourceState = resources.foldLeft(resourceState){ case (rs, r) =>
      r._2.read.foldLeft(rs){ case (rs, reader) => reader.f(driverState, rs)}}
  }

  def getDriverDiffs(c: APIVirtualDevice.VDCommand) = {
    val diffs = for {
      r <- resources.get(c.resource)
    } yield {
      val s = r.write.foldLeft(driverState) { case (ds,writer) =>
        writer.f(Map(c.resource -> c.stateRequest), ds)
      }
      s.map { case (k,v) =>
        val m = driverState.getOrElse(k, v)
        val d = v.toSet diff m.toSet
        (k, d.toMap)
      }
    }
    diffs.getOrElse(Map())
  }

  def getDriverCommands(diffs: Map[ID, VD.DriverState]) = {
    for {
      (did,stateDiff) <- diffs if stateDiff.nonEmpty
      d <- drivers.get(did)
    } yield {
      APIDeviceDriver.DriverCommand(d.id, stateDiff)
    }
  }

}
