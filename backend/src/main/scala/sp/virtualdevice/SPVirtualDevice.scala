package sp.virtualdevice

import akka.NotUsed
import akka.actor._
import sp.devicehandler._
import sp.domain._
import Logic._
import akka.stream._
import akka.stream.scaladsl._

import SPStreamSupport._

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

    case setup: APISPVD.SPVD =>
      log.debug("Setting up VD")
      log.debug(setup.toString)
      if (vds.contains(setup.vd)){
        log.debug("VD already exists")
        // publish(APIVirtualDevice.topicResponse, SPMessage.makeJson(updH, APISP.SPError(s"VD with id ${setup.id} already exist")))
      } else {
        val a = context.actorOf (VirtualDevice.props (setup) )
        vds += setup.vd -> a
        context.watch (a)
        // publish (APIVirtualDevice.topicResponse, SPMessage.makeJson (updH, APISP.SPDone () ) )
      }




    case x: String =>
      val mess = SPMessage.fromJson(x)
      for {
        m <- mess
        h <- m.getHeaderAs[SPHeader] // add filter here if we want multiple makers
        b <- m.getBodyAs[APIVirtualDevice.Request]
      } yield {
        val updH = h.swapToAndFrom()
        b match {

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
  def props(setup: APISPVD.SPVD) = Props(classOf[VirtualDevice], setup)
}

class VirtualDevice(setup: APISPVD.SPVD) extends Actor
    with ActorLogging
    with sp.service.ServiceCommunicationSupport
    with sp.service.MessageBussSupport {

  var forceTable: Map[ID, SPValue] = Map()
  var forceEvents: List[sp.runners.RunnerLogic.FireEvent] = List()

  import context.dispatcher
  implicit val materializer = ActorMaterializer() // maybe send this in another way...

  val id = setup.vd

  subscribe(APIVirtualDevice.topicRequest)
  subscribe(APIDeviceDriver.topicResponse)

  val runner = sp.runners.RunnerPipeline(
    operations = setup.runner.operations,
    transitionSystem = setup.runner.transitionSystem,
    initialState = SPState("initial state", setup.runner.initialState),
    name = setup.vd.toString,
    system = context.system
  )

  // starting in "pause" mode with automatic reset
  // TODO: these id:s should come from outside...
  runner.makeTransitionsControlled(List(AbilityRunnerTransitions.AbilityTransitions.enabledToStarting.id))
  runner.makeTransitionsUnControlled(List(AbilityRunnerTransitions.AbilityTransitions.finToNotEnabled.id))

  val resourceSources = SPStreamSupport.mergeSources(setup.resources.map(r=>r.inputs).flatten)
  val resourceSinks = SPStreamSupport.mergeSinks(setup.resources.map(r=>r.outputs).flatten)

  // twice per second, let frontend know the state
  val limitFrontend = normalizeRate[APISPVD.State](500 millis)
  val frontendSink = Sink.foreach[APISPVD.State] { s =>
    val header = SPHeader(from = id.toString)
    val body = sp.devicehandler.APIVirtualDevice.StateEvent("", id, s)
    val message = SPMessage.makeJson(header, body)
    publish(sp.devicehandler.APIVirtualDevice.topicResponse, message)
  }
  val frontendFlow = limitFrontend.to(frontendSink)

  // keep-alive source
  val ticker = Source.tick(2500 millis, 2500 millis, Map())

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
        b <- m.getBodyAs[APIVirtualDevice.Request]
      } yield {
        val updH = h.swapToAndFrom()
        b match {

          case APIVirtualDevice.StopAuto =>
            println("Stopping auto")
            publish (APIVirtualDevice.topicResponse, SPMessage.makeJson (updH, APISP.SPACK () ) )
            runner.makeTransitionsControlled(List(AbilityRunnerTransitions.AbilityTransitions.enabledToStarting.id))
            publish (APIVirtualDevice.topicResponse, SPMessage.makeJson (updH, APISP.SPDone () ) )


          case APIVirtualDevice.StartAuto =>
            println("Starting auto")
            publish (APIVirtualDevice.topicResponse, SPMessage.makeJson (updH, APISP.SPACK () ) )
            runner.makeTransitionsUnControlled(List(AbilityRunnerTransitions.AbilityTransitions.enabledToStarting.id))
            publish (APIVirtualDevice.topicResponse, SPMessage.makeJson (updH, APISP.SPDone () ) )

          case APIVirtualDevice.SetForceTable(force, events) =>
            println("Setting force table")
            publish (APIVirtualDevice.topicResponse, SPMessage.makeJson (updH, APISP.SPACK () ) )
            forceTable = force
            forceEvents = events.map{ case (id, spval) => sp.runners.RunnerLogic.FireEvent(spval, id) }.toList
            publish (APIVirtualDevice.topicResponse, SPMessage.makeJson (updH, APISP.SPDone () ) )

          case APIVirtualDevice.TerminateVD(killid) if killid == id =>
            killSwitch.shutdown()

          case APIVirtualDevice.TerminateAllVDs =>
            killSwitch.shutdown()


          case _ =>

        }
      }

    case _ =>
  }
}
