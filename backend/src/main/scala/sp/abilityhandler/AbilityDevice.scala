package sp.abilityhandler

import sp.domain._
import sp.domain.Logic._
import akka.actor._

// TODO: 180311: Currently we do not have a synchronized state in the
// TODO: abilities. If guaranteed booking is needed, it must be done on the
// TODO: operation layer


/**
  * Probably merge with the VD later...
  */
class AbilityHandlerMaker extends Actor
  with ActorLogging
  with AbilityLogic
  with sp.service.ServiceCommunicationSupport
  with sp.service.MessageBussSupport {

  import context.dispatcher
  subscribe(APIAbilityHandler.topicRequest)

  // Setting up the status response that is used for identifying the service in the cluster
  val statusResponse = AbilityHandler.attributes.copy(
    instanceName = "AbilityHandlerMaker"
  )

  var ahs: Map[ID, ActorRef] = Map()

  override def receive = {
    //case x if {log.debug(s"ability handler maker got: $x"); false} => false
    case x: String =>
      for {
        m <- SPMessage.fromJson(x)
        h <- m.getHeaderAs[SPHeader] // add filter here if we want multiple makers
        b <- m.getBodyAs[APIAbilityHandler.Request]
      } yield {
        val updH = h.swapToAndFrom()
        b match {
          case setup : APIAbilityHandler.SetUpAbilityHandler =>
            log.debug("Setting up an ability handler")
            log.debug(setup.toString)
            if (ahs.contains(setup.id)){
              publish(APIAbilityHandler.topicResponse, SPMessage.makeJson(updH, APISP.SPError(s"Abilityhandler with id ${setup.id} already exist")))
            } else {
              val a = context.actorOf(AbilityHandler.propsHandler(setup.name, setup.id, setup.vd))
              ahs += setup.id -> a
              context.watch(a)
              a ! APIAbilityHandler.SetUpAbilities(setup.abilities, setup.handshake) // no need for jsonify since this is also matched in AbilityHandler
              publish(APIAbilityHandler.topicResponse, SPMessage.makeJson(updH, APISP.SPDone()))
            }
          case APIAbilityHandler.TerminateAllAbilities =>
            ahs.foreach(a =>  { a._2 ! PoisonPill})
            publish(APIAbilityHandler.topicResponse, SPMessage.makeJson(updH, APISP.SPDone()))
          case _=>
        }
      }

    case Terminated(x) => ahs = ahs.filterNot(_._2 == x)
      if (ahs.isEmpty) publish(APIAbilityHandler.topicResponse, SPMessage.makeJson(SPHeader(from = APIAbilityHandler.service), APIAbilityHandler.AbilitiesTerminated))
    case other =>
  }

}