package sp.abilityhandler

import java.util.UUID

import sp.domain._
import sp.domain.Logic._
import akka.actor._
import akka.persistence._

import scala.util.Try


// TODO: 180311: Currently we do not have a synchronized state in the
// TODO: abilities. If guaranteed booking is needed, it must be done on the
// TODO: operation layer


object AbilityHandler {
  def props = Props(classOf[AbilityHandlerMaker])
  def propsHandler(name: String, id: UUID, vd: UUID) = Props(classOf[AbilityHandler], name, id, vd)

  import sp.domain.SchemaLogic._

  case class AbilityHandlerRequest(request: APIAbilityHandler.Request)
  case class AbilityHandlerResponse(response: APIAbilityHandler.Response)

  val sres: com.sksamuel.avro4s.SchemaFor[AbilityHandlerResponse] = com.sksamuel.avro4s.SchemaFor[AbilityHandlerResponse]
  val sreq: com.sksamuel.avro4s.SchemaFor[AbilityHandlerRequest] = com.sksamuel.avro4s.SchemaFor[AbilityHandlerRequest]


  val apischema = SPAttributes() // makeMeASchema(sreq(), sres())


  val attributes: APISP.StatusResponse = APISP.StatusResponse(
    service = APIAbilityHandler.service,
    instanceID = Some(ID.newID),
    instanceName = "",
    tags = List("ability", "virtual device", "vd", "runtime", "communication"),
    api = apischema,
    version = 1,
    attributes = SPAttributes.empty
  )
}


import sp.operationmatcher.{API => omapi}
import sp.devicehandler._


/**
  * Probably merge with the VD later...
  */
class AbilityHandlerMaker extends Actor
  with ActorLogging
  with AbilityLogic
  with AbilityComm
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
      val mess = SPMessage.fromJson(x)
      for {
        m <- mess
        h <- m.getHeaderAs[SPHeader] // add filter here if we want multiple makers
        b <- m.getBodyAs[APIAbilityHandler.Request] if b.isInstanceOf[APIAbilityHandler.SetUpAbilityHandler]
        setup = b.asInstanceOf[APIAbilityHandler.SetUpAbilityHandler]
      } yield {
        log.debug("Setting up an ability handler")
        log.debug(setup.toString)
        val updH = h.swapToAndFrom()
        if (ahs.contains(setup.id)){
          publish(APIAbilityHandler.topicResponse, SPMessage.makeJson(updH, APISP.SPError(s"Abilityhandler with id ${setup.id} already exist")))
        } else {
          val a = context.actorOf(AbilityHandler.propsHandler(setup.name, setup.id, setup.vd))
          ahs += setup.id -> a
          context.watch(a)
          a ! APIAbilityHandler.SetUpAbilities(setup.abilities, setup.handshake) // no need for jsonify since this is also matched in AbilityHandler
          publish(APIAbilityHandler.topicResponse, SPMessage.makeJson(updH, APISP.SPDone()))
        }
      }
    case Terminated(x) => ahs = ahs.filter(_._2 == x)
  }



}


// This actor will keep track of the abilities and parse all messages from the VD
class AbilityHandler(name: String, handlerID: ID, vd: ID) extends Actor
    with ActorLogging
    with AbilityLogic
    with AbilityComm
    with sp.service.ServiceCommunicationSupport
    with sp.service.MessageBussSupport {
  import context.dispatcher


  // Internal state of the ability
  case class AbilityStorage(ability: APIAbilityHandler.Ability, actor: ActorRef, ids: Set[ID] = Set(), current: Option[AbilityStateChange] = None)

  // Internal state of the ability handler
  var abilities: Map[ID, AbilityStorage] = Map()
  var resources: List[VD.Resource] = List()
  var state: Map[ID, SPValue] = Map()


  subscribe(APIAbilityHandler.topicRequest)
  subscribe(APIVirtualDevice.topicResponse)

  // Setting up the status response that is used for identifying the service in the cluster
  val statusResponse = AbilityHandler.attributes.copy(
    instanceName = this.name,
    instanceID = Some(handlerID),
    attributes = SPAttributes("vd" -> vd)
  )

  // starts waiting for ping requests from service handler
  triggerServiceRequestComm(statusResponse)


  val getVD = makeMess(SPHeader(from = handlerID.toString, to = vd.toString), APIVirtualDevice.GetVD)
  publish(APIVirtualDevice.topicRequest, getVD)

  override def receive = {

    case x: String => handleRequests(x)
    case x if {log.debug(s"ABH from an ability got: $x"); false} => false

    case CanNotStart(req, abID, error) =>
      val h = SPHeader(from = handlerID.toString)
      publish(APIAbilityHandler.topicResponse, makeMess(h, APISP.SPError(s"ability $abID couldn't start. $error")))
      publish(APIAbilityHandler.topicResponse, makeMess(h, APISP.SPDone()))

    case x @ AbilityStateChange(abID, s, cnt, req) =>
      val h = SPHeader(from = handlerID.toString, reqID = req.getOrElse(ID.newID))
      abilities.get(abID).foreach{ as =>
        abilities += abID -> as.copy(current = Some(x))
      }
      val abilityState = SPAttributes(
        "state" -> s,
        "counter" -> cnt
      )
      val b = APIAbilityHandler.AbilityState(abID, Map(abID -> abilityState))
      publish(APIAbilityHandler.topicResponse, makeMess(h, b))

      req.foreach{ req =>
        val res = s match {
          case "executing" =>
            publish(APIAbilityHandler.topicResponse, makeMess(h, APIAbilityHandler.AbilityStarted(abID)))
          case "finished" =>
            publish(APIAbilityHandler.topicResponse, makeMess(h, APIAbilityHandler.AbilityCompleted(abID, Map())))
            publish(APIAbilityHandler.topicResponse, makeMess(h, APISP.SPDone()))
          case _ => Unit

        }

      }

    case StateUpdReq(abID, s) =>
      val res = resources.filter(r => r.things.intersect(s.keySet).nonEmpty)
      val toSend = res.map{r =>
        val h = SPHeader(from = handlerID.toString, to = vd.toString, reply = SPValue(handlerID))
        val b = APIVirtualDevice.VDCommand(r.id, s.filter(kv => r.things.contains(kv._1)))
        publish(APIVirtualDevice.topicRequest, makeMess(h, b))
      }

    case StateIsMissingIDs(abID, ids) =>
      val h = SPHeader(from = handlerID.toString)
      val errorAttr = SPAttributes(
        "ability" -> abilities.get(abID).map(_.ability.name),
        "id" -> abID, "missingThings" -> ids)

      publish("spevents", makeMess(h,
        APISP.SPError("Ability has ids that is not found in the state. Either the VD is unavailable or something is wrong",
        errorAttr)
      ))


    case APIAbilityHandler.SetUpAbilities(xs, hand) => xs.foreach(setupNewAbility)

  }


  def handleRequests(x: String): Unit = {
    val mess = SPMessage.fromJson(x)

    matchRequests(mess)
    matchVDMessages(mess)
    matchOMRequest(mess)

  }


  def matchRequests(mess: Option[SPMessage]) = {
    extractRequest(mess, handlerID, name) foreach { case (h, b) =>
      log.debug("ABH req: " +b)
      val updH = h.swapToAndFrom()

      // Message was to me so i send an SPACK
      publish(APIAbilityHandler.topicResponse, makeMess(updH, APISP.SPACK()))

      b match {
        case APIAbilityHandler.StartAbility(id, params, attr) =>
          abilities.get(id) match {
            case Some(a) =>
              a.actor ! StartAbility(state, h.reqID, params, attr)
            case None =>
              publish(APIAbilityHandler.topicResponse, makeMess(updH, APISP.SPError(s"ability $id does not exists in this handler")))
              publish(APIAbilityHandler.topicResponse, makeMess(updH, APISP.SPDone()))

          }

        case APIAbilityHandler.ForceResetAbility(id) =>
          abilities.get(id) match {
            case Some(a) =>
              a.actor ! ResetAbility(state)
            case None =>
              publish(APIAbilityHandler.topicResponse, makeMess(updH, APISP.SPError(s"ability $id does not exists in this handler")))
          }
          publish(APIAbilityHandler.topicResponse, makeMess(updH, APISP.SPDone()))

        case APIAbilityHandler.ForceResetAllAbilities =>
          val r = ResetAbility(state)
          abilities.foreach(kv => kv._2.actor ! r)
          publish(APIAbilityHandler.topicResponse, makeMess(updH, APISP.SPDone()))

        case APIAbilityHandler.ExecuteCmd(cmd) =>
          val things = abilities.map { case (id,a) =>
          }
        // to be implemented

        case APIAbilityHandler.GetAbilities =>
          val xs = abilities.map(_._2.ability).toList

          val abs = abilities.map(a=>(a._2.ability.id,a._2.ability.name)).toList

          log.debug("got getabilities request")
          publish(APIAbilityHandler.topicResponse, makeMess(updH, APIAbilityHandler.Abilities(xs)))
          publish(APIAbilityHandler.topicResponse, makeMess(updH, APIAbilityHandler.Abs(abs)))

          publish(APIAbilityHandler.topicResponse, makeMess(updH, APISP.SPDone()))

          abilities.foreach(a => a._2.actor ! GetState)


        case APIAbilityHandler.SetUpAbility(ab, hand) =>
          log.debug(ab.toString)
          setupNewAbility(ab)
          publish(APIAbilityHandler.topicResponse, makeMess(updH, APISP.SPDone()))

        case APIAbilityHandler.SetUpAbilities(xs, hand) =>
          log.debug(xs.toString)

          xs.foreach { ab =>
            setupNewAbility(ab)
          }
          publish(APIAbilityHandler.topicResponse, makeMess(updH, APISP.SPDone()))
      }
    }
  }

  def setupNewAbility(ab: APIAbilityHandler.Ability) = {
    // TODO: Handle if ability already exists
    val ids = idsFromAbility(ab)
    val act = context.actorOf(AbilityActor.props(ab))
    abilities += ab.id -> AbilityStorage(ab, act, ids)
    act ! NewState(filterState(ids, state))
  }

  def filterState(ids: Set[ID], state: Map[ID, SPValue]) = state.filter(kv => ids.contains(kv._1))

  def matchOMRequest(mess: Option[SPMessage]) = {
    extractOMRequest(mess) foreach { case (h, b) =>
      log.debug("ABH from OP matcher: " +b)
      b match {
        case omapi.Find(pairs: Map[String, SPValue]) =>
          val updH = h.swapToAndFrom()
          publish(APIAbilityHandler.topicResponse, makeMess(updH, APISP.SPACK()))
          val abs = abilities.map(_._2.ability).toSet
          val a = abs.filter{ a =>
            val abkeys = a.attributes.getAs[Map[String, SPValue]]("pairs").getOrElse(Map())
            pairs.forall{ kv => abkeys.exists(_ == kv) }
          }
          val an = (abs.filter{ a =>
            val abkeys = a.attributes.getAs[Map[String, SPValue]]("pairs").getOrElse(Map())
            pairs.forall{ case (k,_) => abkeys.contains(k) }
          }).diff(a)

          val msg = makeMess(updH, omapi.Matches(a.toList,an.toList))
          publish(APIAbilityHandler.topicResponse, msg)
          publish(APIAbilityHandler.topicResponse, makeMess(updH, APISP.SPDone()))
      }
    }
  }

  def matchVDMessages(mess: Option[SPMessage]) = {
    extractVDReply(mess, handlerID, vd.toString) foreach { case (h, b) =>
      log.debug("ABH from VD: " +b)
      b match {
        case x @ APIVirtualDevice.StateEvent(r, rID, s, d) =>
          state = state ++ s

          // Add filters if we need it later. Probably is better that all abilities has
          // the complete state
          //val f = abilities.filter(kv => kv._2.ids.intersect(s.keySet).nonEmpty)
          //f.foreach{kv => kv._2.actor ! NewState(filterState(kv._2.ids, state))}

          // The no filter version
          abilities.foreach(kv => kv._2.actor ! NewState(state))

        case x: APIVirtualDevice.TheVD =>
          resources = x.resources.map(_.r)
          state = x.resources.foldLeft(state)(_ ++ _.state)
          abilities.foreach{kv => kv._2.actor ! NewState(filterState(kv._2.ids, state))}
        case x =>
      }
    }

  }

}




// Internal api between abilities and handler
case class StartAbility(state: Map[ID, SPValue], reqID: ID, params: Map[ID, SPValue], attributes: SPAttributes = SPAttributes())
case class ResetAbility(state: Map[ID, SPValue])
case object GetIds
case class NewState(s: Map[ID, SPValue])
case object UnAvailable
case object GetState

case class AbilityIds(ability: ID, ids: Set[ID])
case class CanNotStart(reqID: ID, ability: ID, error: String)
case class AbilityStateChange(ability: ID, state: String, cnt: Long, reqID: Option[ID])
case class StateIsMissingIDs(ability: ID, xs: Set[ID])
case class StateUpdReq(ability: ID, state: Map[ID, SPValue])


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
      val res = start(s ++ p)
      res.collect {
        case updS if updS != s =>
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







// TODO 180228: Clean up and update this state handler!

// The various states that an ability can be in
object AbilityState {
    val unavailable = "unavailable"
    val notEnabled = "notEnabled"
    val enabled = "enabled"
    val starting = "starting"
    val executing = "executing"
    val finished = "finished"
    val forcedReset = "forcedReset"
    val failed = "failed"
}

// TODO: Merge the state of the abilities into the VD-state (when the VD is moved)
trait AbilityActorLogic extends AbilityLogic{
  val ability: APIAbilityHandler.Ability
  lazy val ids = idsFromAbility(ability)

  import AbilityState._

  var state: String = unavailable
  var count: Long = 0
  var currentCaller = SPAttributes()


  def makeUnavailable() = state = unavailable
  def makeAvailable() = state = notEnabled

  def start(s: Map[ID, SPValue]): Option[Map[ID, SPValue]] = {
    val tH = evalState(s, "start")
    if (state == starting || state == executing){
      Some(tH._2)
    } else None
  }

  def reset(s: Map[ID, SPValue]) = {
    val tH = evalState(s, "reset")
    tH._2
  }


  /**
    * Evaluates the ability state and updates it if needed
    * @param s Current state
    * @param cmd to start, reset or fail the ability, Leave blank if no cmd
    * @return
    */
  def evalState(s: Map[ID, SPValue], cmd: String = ""): (Option[String], Map[ID, SPValue]) = {
    val theState = SPState(state = s)
    val abilityState = updateState(theState, cmd)

    val newAState = if (abilityState._1.isEmpty) None else Some(abilityState._1)
    val newRState = if (theState != abilityState._2) Some(abilityState._2.state) else None

    newAState.foreach(x => state = x)
    (newAState, abilityState._2.state)
  }

  /**
    * The main state machine, taking the state and a command (start or reset) and updated the ability state
    * To have a synced execution, i.e. that there is a well defined state in the real system that
    * defined that the ability is execution, set the "syncedExecution" attributes to true in
    * the abilities attributes.
    * Set the "syncedFinished" attributes to true if there is a well defined finshed state
    * @param theState Current state
    * @param cmd Either "start" or "reset" (and fail soon)
    * @return (String, SPState), Where the string states the new state, or empty if
    *         the state was not changed. And the state is the new updated state
    */
  def updateState(theState: SPState, cmd: String): (String, SPState) = state match {
    case x if cmd == "start" &&
      (x == enabled || x == notEnabled || x == unavailable) &&
      ability.preCondition.eval(theState) =>

      if (ability.started.eval(theState)) // skipping starting for simple abilities
        (executing, ability.started.next(ability.preCondition.next(theState)))
      else
        (starting, ability.preCondition.next(theState))

    case x if cmd == "reset" && !(x == forcedReset || x == unavailable || x == enabled || x == notEnabled) =>
      (forcedReset, theState)

    case x if x == forcedReset =>
      val updS =  ability.resetCondition.next(theState)
      (checkEnabled(updS), updS)

    case x if x == unavailable && theState.state.nonEmpty =>
      (notEnabled, theState)

    case x if x != executing && syncedExecution(ability) && ability.started.eval(theState) =>
      (executing, theState)
    case x if x != finished && syncedFinished(ability) && ability.postCondition.eval(theState) =>
      (finished, theState)

    case x if x == notEnabled && ability.preCondition.eval(theState) =>
      (enabled, theState)
    case x if x == enabled && !ability.preCondition.eval(theState) =>
      (notEnabled, theState)


    case x if x == starting && ability.started.eval(theState) =>
      (executing, ability.started.next(theState))
    case x if x == executing && ability.postCondition.eval(theState) =>
      (finished, ability.postCondition.next(theState))
    case x if x == finished && ability.resetCondition.eval(theState) =>
      val updS = ability.resetCondition.next(theState)
      (checkEnabled(updS), updS)


    //case x if x == failed => // not yet used. fail if driver fails

    case _ => ("", theState) // No change

  }

  def checkEnabled(tS: SPState) = if (ability.preCondition.eval(tS)) enabled else notEnabled



  def createNotStartingErrorMessage() = {
    s"state: $state"
  }

}

trait AbilityLogic {


  def idsFromAbility(a: APIAbilityHandler.Ability) = {
    Set(a.preCondition,
      a.postCondition,
      a.started, a.resetCondition).flatMap(extractVariables) ++
      a.parameters ++ a.result
  }

  def extractVariables(p: Condition) = {
    fromGuard(p.guard) ++ fromAction(p.action)
  }

  def fromGuard(p: Proposition): List[ID] = {
    p match {
      case AND(xs) => xs.flatMap(fromGuard)
      case OR(xs) => xs.flatMap(fromGuard)
      case NOT(x) => fromGuard(x)
      case pe: PropositionEvaluator =>
        val xs = List(pe.left, pe.right)
        xs.collect{
          case SVIDEval(id) => id

        }
      case x => List()
    }
  }


  def fromAction(a: List[Action]): List[ID] = {
    a.map(_.id) ++  a.map(_.value).collect{
      case ASSIGN(id) => id
    }
  }

  /**
    * Check the attributes if this ability has a well defined executing
    * state. If so, it should jump to it if the starting guard is true
    * @param a
    */
  def syncedExecution(a: APIAbilityHandler.Ability): Boolean = {
    a.attributes.getAs[Boolean]("syncedExecution").getOrElse(false)
  }

  /**
    * Check the attributes if this ability has a well defined finished
    * state. If so, it should jump to it if the postcondition guard is true
    * @param a
    */
  def syncedFinished(a: APIAbilityHandler.Ability): Boolean = {
    a.attributes.getAs[Boolean]("syncedFinished").getOrElse(false)
  }


}


trait AbilityComm {

  def extractRequest(mess: Option[SPMessage], instanceID: ID, name: String) = for {
    m <- mess
    h <- m.getHeaderAs[SPHeader] if h.to == instanceID.toString || h.to == name || h.to == AbilityHandler.attributes.service
    b <- m.getBodyAs[APIAbilityHandler.Request]
  } yield (h, b)

  def extractVDReply(mess: Option[SPMessage], instanceID: ID, vd: String) = {
    for {
    m <- mess
    h <- m.getHeaderAs[SPHeader] if h.from.contains(vd) || h.reply == SPValue(instanceID)
    b <- m.getBodyAs[APIVirtualDevice.Response]
  } yield (h, b)}

  def extractServiceRequest(mess: Option[SPMessage]) = for {
    m <- mess
    h <- m.getHeaderAs[SPHeader]
    b <- m.getBodyAs[APISP] if b == APISP.StatusRequest
  } yield {
    (h, b)
  }

  def extractOMRequest(mess: Option[SPMessage]) = for {
    m <- mess
    h <- m.getHeaderAs[SPHeader]
    b <- m.getBodyAs[omapi.Request] if h.to == APIAbilityHandler.service
  } yield {
    (h, b)
  }


  def makeMess(h: SPHeader, b: APIAbilityHandler.Response) = SPMessage.makeJson[SPHeader, APIAbilityHandler.Response](h, b)
  def makeMess(h: SPHeader, b: APIVirtualDevice.Request) = SPMessage.makeJson[SPHeader, APIVirtualDevice.Request](h, b)
  def makeMess(h: SPHeader, b: APISP) = SPMessage.makeJson[SPHeader, APISP](h, b)
  def makeMess(h: SPHeader, b: omapi.Response) = SPMessage.makeJson[SPHeader, omapi.Response](h, b)
}