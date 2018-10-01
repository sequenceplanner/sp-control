package sp.virtualdevice

import akka.NotUsed
import akka.actor._
import sp.devicehandler._
import sp.domain._
import Logic._
import akka.stream._
import akka.stream.scaladsl._
import sp.abilityhandler.{APIAbilityHandler, AbilityHandler}





// The VD maker
// makes a VD and then sends the ability to it when we get the make ability handler
// this should be merged later KB:180928

class SPVirtualDeviceMaker extends Actor
  with ActorLogging
  with sp.service.ServiceCommunicationSupport
  with sp.service.MessageBussSupport {

  // Reusing the old info
  triggerServiceRequestComm(VirtualDeviceInfo.attributes.copy(
    service = "NewVirtualDeviceMaker")
  )

  subscribe(APIVirtualDevice.topicRequest)
  subscribe(APIAbilityHandler.topicRequest)


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

          case setup : APIVirtualDevice.SetUpVD =>
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

      // Just forward to the vd since abilities are there now
      // När jag tänker på detta så behöver vi ändra där dessa skapas så att vi skickar abilities när vi skapar VDn ovan. Tror bara vi skickar dessa från några få ställen, så enklast att ändra på det där. Annars kan vi få dessa innan SetUpVD
      for {
        m <- SPMessage.fromJson(x)
        h <- m.getHeaderAs[SPHeader] // add filter here if we want multiple makers
        b <- m.getBodyAs[APIAbilityHandler.Request]
      } yield {
        val updH = h.swapToAndFrom()
        b match {
          case setup : APIAbilityHandler.SetUpAbilityHandler =>
            log.debug(setup.toString)
            if (!vds.contains(setup.vd)){
              publish(APIAbilityHandler.topicResponse, SPMessage.makeJson(updH, APISP.SPError(s"The VD with the id ${setup.vd} do not exist")))
            } else {
              val a = vds(setup.vd)
              a ! APIAbilityHandler.SetUpAbilities(setup.abilities, setup.handshake) // no need for jsonify since this is also matched in AbilityHandler
              publish(APIAbilityHandler.topicResponse, SPMessage.makeJson(updH, APISP.SPDone()))
            }

          case _=>
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
  def props(setup: APIVirtualDevice.SetUpVD) = Props(classOf[VirtualDevice], setup)
}

class VirtualDevice(setup: APIVirtualDevice.SetUpVD) extends Actor with AbilityRunnerTransitions
  with ActorLogging
  with VirtualDeviceLogic
  with sp.service.ServiceCommunicationSupport
  with sp.service.MessageBussSupport {

  import context.dispatcher
  implicit val materializer = ActorMaterializer() // maybe send this in another way...

  val name = setup.name
  val id = setup.id

  subscribe(APIVirtualDevice.topicRequest)
  subscribe(APIDeviceDriver.topicResponse)



  // set up the VD with the data. Should include ops and the message structure in some way. Or we try with one two one mapping
  // Setup


  val dummyInit = setup.resources.flatMap(r => r.things.map(_ -> SPValue("noValue"))).toMap // here we need to change teh def of resources so we get things from the model
  val initState = SPState("runnerState", dummyInit) // här får vi skapa ett bättre init state där vi också inkl operations när de kommer

  val runner = sp.runners.RunnerPipeline(
    operations = List(), // vi får tyvärr inte dessa här utan de kommer via abilities. Det kanske inte fungerar om abilitymaker komemr före VDmaker. Men vi testar såhär. Annars får vi uppdatera där vi går från modellen till dessa setup messages
    transitionSystem = transitionSystem, // def i AbilityRunnerTransitions trait nedan
    initialState = initState,
    name = setup.name,
    system = context.system
  )

  // add StateUpd to que and plug in flows and a sink to send SPState where you want
  val runnerPipelineSource =
    Source.queue[sp.runners.StateUpd](100, akka.stream.OverflowStrategy.backpressure)
    .via(runner.runnerFlow(Some(500 milliseconds))) // den tickar...


  // go from SPState to resources with state
  //val resourceFlow

  // go from resources and state to driver commands
  // val driverFlow

  // Also add a check if the state for the resource has not changed and only forward if new
  // after that, we can add a throttle


  val pipeline = runnerPipelineSource
    .toMat(Sink.ignore)(Keep.both) // make a nice sink to use the result here . Should not go back to this actor
    .run()

  val queue = pipeline._1


  // Setting UP
  override def receive = {
    case x: APIAbilityHandler.SetUpAbilities =>
      // Vi tar bort detta direkt och lägger in det i VD setup...


    case x =>
      // receive driver data
      // receive other stuff from upper systems when needed

      // receive commands to add or remove ops. events, etc.



      val res = queue.offer(sp.runners.StateUpd(SPState("test", Map()), List()))

  }



}


// Inte testad! Måste skriva test för denna så att det fungerar som tänkt
trait AbilityRunnerTransitions {
  import sp.runners.RunnerLogic._

  // states
  val notEnabled = "notEnabled"
  val enabled = "enabled"
  val starting = "starting"
  val executing = "executing"
  val finished = "finished"

  // kinds
  val pre = "pre"
  val started = "started"
  val post = "post"
  val postAlternative = "postAlternative"
  val reset = "reset"

  val notEnabledToEnabled = OperationTransition(
    states = Set(notEnabled),
    conditionKind =  pre,
    nextState = executing,
    event = None,
    alwaysTrueIfNoConditions = true,
    enableAlternatives = false,
    onlyGuard = true,
    negateGuard = false
  )
  val enabledToNotEnabled = OperationTransition(
    states = Set(enabled),
    conditionKind =  pre,
    nextState = notEnabled,
    event = None,
    alwaysTrueIfNoConditions = false,
    enableAlternatives = false,
    onlyGuard = true,
    negateGuard = true  // this will go back if pre guard is false when in enabled
  )
  val enabledToStarting = OperationTransition(
    states = Set(enabled),
    conditionKind =  pre,
    nextState = starting,
    event = None, // should be Some("start") after we have tested
    alwaysTrueIfNoConditions = false,
    enableAlternatives = false,
    onlyGuard = false,
    negateGuard = false
  )
  val startingToExec = OperationTransition(
    states = Set(starting),
    conditionKind =  started,
    nextState = executing,
    event = None,
    alwaysTrueIfNoConditions = true,
    enableAlternatives = false,
    onlyGuard = false,
    negateGuard = false
  )

  val execToFinished = OperationTransition(
    states = Set(executing),
    conditionKind =  post,
    nextState = finished,
    event = None,
    alwaysTrueIfNoConditions = true,
    enableAlternatives = false,
    onlyGuard = false,
    negateGuard = false
  )
  val execToFinishedAlt = OperationTransition(
    states = Set(executing),
    conditionKind =  postAlternative,
    nextState = finished,
    event = None,
    alwaysTrueIfNoConditions = false,
    enableAlternatives = true,
    onlyGuard = false,
    negateGuard = false
  )
  val finToNotEnabled = OperationTransition(
    states = Set(finished),
    conditionKind =  reset,
    nextState = notEnabled,
    event = None, // Some("reset")
    alwaysTrueIfNoConditions = false,
    enableAlternatives = false,
    onlyGuard = false,
    negateGuard = false
  )
  val forceReset = OperationTransition(
    states = Set(starting, executing, finished),
    conditionKind =  "ShouldNotHaveAnyConditions",
    nextState = notEnabled,
    event = Some("forceReset"),
    alwaysTrueIfNoConditions = true,
    enableAlternatives = false,
    onlyGuard = false,
    negateGuard = false
  )


  val transitionSystem = List(
    notEnabledToEnabled,
      enabledToNotEnabled,
      enabledToStarting,
      startingToExec,
      execToFinished,
      execToFinishedAlt,
      finToNotEnabled,
      forceReset
  )


}
