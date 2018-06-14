package sp.abilityhandler

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import sp.domain.{ID, SPValue}
object AbilityActor {
  def props(ability: APIAbilityHandler.Ability) =
    Props(classOf[AbilityActor], ability)
}

class AbilityActor(val ability: APIAbilityHandler.Ability) extends Actor
  with AbilityActorLogic
  with ActorLogging
{
  var reqID: Option[ID] = None

  override def receive = {
    case GetIds => sender() ! AbilityIds(ability.id, ids)
    case UnAvailable =>
      makeUnavailable()
      sendAbilityState(sender())

    case x @ StartAbility(s, id, p, attr) =>
      log.debug("STARTING ABILITY")
      log.debug(x.toString)
      val res = start(s)
      res.collect {
        case updS if updS != s || p.nonEmpty =>
          reqID = Some(id)
          log.debug(updS.toString())
          if (p.nonEmpty) sender() ! StateUpdReq(ability.id, p)
          sender() ! StateUpdReq(ability.id, updS)
          sendAbilityState(sender())

          log.debug("")
          log.info("StartAbility v v v" + ability.name)
          log.debug("the state: " + s)
          log.debug("new ability state: " + state)
          log.debug("ability updated state: " + updS)
          log.debug("StartAbility END")
          log.debug("")
        case _ =>
          sender() ! CanNotStart(id, ability.id, createNotStartingErrorMessage())

        //fix timeout here if needed
      }

    //checkAndSend(res.getOrElse(s), sender())

    case ResetAbility(s) =>
      val res = reset(s)
      if (res != s)  sender() ! StateUpdReq(ability.id, res)
      sendAbilityState(sender())

    //checkAndSend(res.getOrElse(s), sender())

    case NewState(s) =>
      val missingIDs = ids.diff(s.keySet)
      if (missingIDs.nonEmpty){
        sender() ! StateIsMissingIDs(ability.id, missingIDs)
      }

      // Always checking twice if the ability can jump multiple states
      // But limits too two to avoid free wheeling
      checkAndSend(s, sender())
      checkAndSend(s, sender())
    case GetState =>
      sendAbilityState(sender())
  }

  def sendAbilityState(to: ActorRef) = {
    to ! AbilityStateChange(ability.id, state, count, reqID)
  }

  def checkAndSend(s: Map[ID, SPValue], to: ActorRef): Unit = {
    val res = evalState(s)

    val changed = res._2 != s

    val print = res._1.isDefined || changed
    if (print) log.debug("checkAndSend v v v")
    if (print) log.debug("the state: " + s)
    if (print) log.debug("new ability state: " + res._1)
    if (print) log.debug("ability updated state: " + res._2)
    if (print) log.debug("checkAndSend END")


    res._1.foreach { updS =>
      sendAbilityState(to)
    }
    if (changed)
      to ! StateUpdReq(ability.id, res._2)

  }


}