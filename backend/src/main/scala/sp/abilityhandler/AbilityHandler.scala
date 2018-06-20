package sp.abilityhandler

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import sp.devicehandler.{APIVirtualDevice, VD}
import sp.domain.APISP.StatusResponse
import sp.domain._
import sp.domain.Logic._
import sp.operationmatcher.API

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

/**
  * This actor keeps track of all abilities. It also handles messages
  * from the Virtual Device.
  */
class AbilityHandler(name: String, handlerID: ID, virtualDeviceID: ID) extends Actor
  with ActorLogging
  with AbilityLogic
  with sp.service.ServiceCommunicationSupport
  with sp.service.MessageBussSupport {
  import context.dispatcher


  // Internal state of the ability
  case class AbilityStorage(ability: APIAbilityHandler.Ability, actor: ActorRef, ids: Set[ID] = Set(), current: Option[AbilityStateChange] = None)

  type AbilityID = ID
  case class State(
                    abilityStates: Map[AbilityID, AbilityStorage] = Map(),
                    resources: List[VD.Resource] = List(),
                    states: Map[ID, SPValue] = Map()
                  ) {
    def abilities: Iterable[APIAbilityHandler.Ability] = abilityStates.values.map(_.ability)
  }

  subscribe(APIAbilityHandler.topicRequest)
  subscribe(APIVirtualDevice.topicResponse)

  {
    // Setting up the status response that is used for identifying the service in the cluster
    val statusResponse: StatusResponse = AbilityHandler.attributes.copy(
      instanceName = this.name,
      instanceID = Some(handlerID),
      attributes = SPAttributes("vd" -> virtualDeviceID)
    )

    // starts waiting for ping requests from service handler
    triggerServiceRequestComm(statusResponse)

    val getVD: String = SPMessage.makeJson(SPHeader(from = handlerID.toString, to = virtualDeviceID.toString), APIVirtualDevice.GetVD)
    publish(APIVirtualDevice.topicRequest, getVD)
  }

  /**
    * Transition the actor to a new state. The new State will be used for all future handleReceive() calls.
    */
  def transitionState(newState: State): Unit = context become handleReceive(newState)

  def handleReceive(state: State): Receive = {
    println(state)
    import APIAbilityHandler.{topicResponse, AbilityStarted, AbilityCompleted, AbilityState}
    {
      case x: String =>
        println("handleReceive() string in AbilityHandler.")
        SPMessage.fromJson(x).foreach { message =>
          val result = message.oneOf[APIAbilityHandler.Request]
            .or[APIVirtualDevice.Response]
            .or[API.Request]

          for (header <- result.header; body <- result.body) body match {
            case req: APIAbilityHandler.Request => onAbilityHandlerRequest(state, header, req)
            case response: APIVirtualDevice.Response => onVirtualDeviceRequest(state, header, response)
            case req: API.Request => onOperationMatcherRequest(state, header, req)
          }
      }

      case x if {log.debug(s"ABH from an ability got: $x"); false} => Unit

      case CanNotStart(_, abID, error) =>
        val header = SPHeader(from = handlerID.toString)
        publish(topicResponse, SPMessage.makeJson(header, APISP.SPError(s"ability $abID couldn't start. $error")))
        publish(topicResponse, SPMessage.makeJson(header, APISP.SPDone()))

      case x @ AbilityStateChange(abilityID, stateString, count, reqID) =>
        val header = SPHeader(
          from = handlerID.toString,
          reqID = reqID.getOrElse(ID.newID)
        )

        transitionState(state.copy(
          abilityStates = state.abilityStates.updateValue(abilityID) { _.copy(current = Some(x)) }
        ))

        val attributes = SPAttributes("state" -> stateString, "counter" -> count)

        val abilityState = AbilityState(abilityID, Map(abilityID -> attributes))
        publish(topicResponse, SPMessage.makeJson(header, abilityState))


        if (reqID.isDefined) {
          stateString match {
            case AbilityStatus.Executing =>
              publish(topicResponse, SPMessage.makeJson(header, AbilityStarted(abilityID)))

            case AbilityStatus.Finished =>
              publish(topicResponse, SPMessage.makeJson(header, AbilityCompleted(abilityID, Map())))
              publish(topicResponse, SPMessage.makeJson(header, APISP.SPDone()))

            case _ => Unit
          }
        }

      case StateUpdReq(_, s) =>
        val res = state.resources.filter(_.things.intersect(s.keySet).nonEmpty)

        res.foreach { resource =>
          val header = SPHeader(from = handlerID.toString, to = virtualDeviceID.toString, reply = SPValue(handlerID))
          val body = APIVirtualDevice.VDCommand(resource.id, s.filter(kv => resource.things.contains(kv._1)))
          publish(APIVirtualDevice.topicRequest, SPMessage.makeJson(header, body))
        }

      case StateIsMissingIDs(abID, ids) =>
        val h = SPHeader(from = handlerID.toString)
        val errorAttr = SPAttributes(
          "ability" -> state.abilityStates.get(abID).map(_.ability.name),
          "id" -> abID,
          "missingThings" -> ids
        )

        val error = APISP.SPError(
          "Ability has ids that is not found in the state. Either the VD is unavailable or something is wrong",
          errorAttr
        )

        publish("spevents", SPMessage.makeJson(h, error))

      case APIAbilityHandler.SetUpAbilities(_abilities, _) =>
        setupAbilityStates(state, _abilities:_*)
    }
  }


  override def receive: Receive = handleReceive(State())

  def onAbilityHandlerRequest(currentState: State, header: SPHeader, req: APIAbilityHandler.Request): Unit = {
    import APIAbilityHandler.{topicResponse, ForceResetAllAbilities}

    def isValidHeader(h: SPHeader): Boolean = {
      Seq(
        name,
        handlerID.toString,
        AbilityHandler.attributes.service
      ).contains(h.to)
    }

    if (isValidHeader(header)) {
      log.debug(s"ABH req: $req")
      val responseHeader = header.swapToAndFrom()

      // Message was to me so i send an SPACK
      publish(topicResponse, SPMessage.makeJson(responseHeader, APISP.SPACK()))

      req match {
        case APIAbilityHandler.StartAbility(id, params, attr) =>
          currentState.abilityStates.get(id) match {
            case Some(a) =>
              a.actor ! StartAbility(currentState.states, header.reqID, params, attr)
            case None =>
              publish(topicResponse, SPMessage.makeJson(responseHeader, APISP.SPError(s"ability $id does not exists in this handler")))
              publish(topicResponse, SPMessage.makeJson(responseHeader, APISP.SPDone()))

          }

        case APIAbilityHandler.ForceResetAbility(id) =>
          currentState.abilityStates.get(id) match {
            case Some(a) =>
              a.actor ! ResetAbility(currentState.states)
            case None =>
              publish(topicResponse, SPMessage.makeJson(responseHeader, APISP.SPError(s"ability $id does not exists in this handler")))
          }
          publish(topicResponse, SPMessage.makeJson(responseHeader, APISP.SPDone()))

        case ForceResetAllAbilities =>
          val r = ResetAbility(currentState.states)
          currentState.abilityStates.foreach { case (_, abilityState) =>  abilityState.actor ! r }
          publish(APIAbilityHandler.topicResponse, SPMessage.makeJson(responseHeader, APISP.SPDone()))

        // TODO to be implemented
        case APIAbilityHandler.ExecuteCmd(cmd) =>
          val things = currentState.abilityStates.map { case (id, a) =>
            Unit
          }

        case APIAbilityHandler.GetAbility(id) =>
          val ability = currentState.abilities.find(_.id == id)
          // make message with header and trigger the response with The Ability
          publish(APIAbilityHandler.topicResponse, SPMessage.makeJson(responseHeader, APIAbilityHandler.TheAbility(ability)))

        case APIAbilityHandler.GetAbilities =>
          log.debug("Received GetAbilities request.")

          val currentAbilities = currentState.abilities.toList
          val abilityData = currentAbilities.map(a => a.id -> a.name)

          publish(topicResponse, SPMessage.makeJson(responseHeader, APIAbilityHandler.Abilities(currentAbilities)))
          publish(topicResponse, SPMessage.makeJson(responseHeader, APIAbilityHandler.AbilitiesByIdAndName(abilityData)))
          publish(topicResponse, SPMessage.makeJson(responseHeader, APISP.SPDone()))

          currentState.abilityStates.values.foreach(_.actor ! GetState)

        case APIAbilityHandler.SetUpAbility(ability, _) =>
          log.debug(ability.toString)
          setupAbilityStates(currentState, ability)
          publish(topicResponse, SPMessage.makeJson(responseHeader, APISP.SPDone()))

        case APIAbilityHandler.SetUpAbilities(_abilities, _) =>
          log.debug(_abilities.toString)

          setupAbilityStates(currentState, _abilities: _*)
          publish(topicResponse, SPMessage.makeJson(responseHeader, APISP.SPDone()))

        case x => println(s"AbilityHandler received unknown message: $x")
      }
    }
  }

  def newAbilityState(ability: APIAbilityHandler.Ability): AbilityStorage = {
    val ids = idsFromAbility(ability)
    val actor = context.actorOf(AbilityActor.props(ability))
    AbilityStorage(ability, actor, ids)
  }

  def setupAbilityStates(currentState: State, _abilities: APIAbilityHandler.Ability*): Unit = {
    val abilityStates = _abilities.map(newAbilityState)
    // Send initial state to the ability actors
    abilityStates.foreach { s => s.actor ! NewState(currentState.states.filterKeys(s.ids.contains)) }

    val newAbilityStates = abilityStates.foldLeft(currentState.abilityStates) { (states, s) => states.updated(s.ability.id, s) }
    transitionState(currentState.copy(abilityStates = newAbilityStates))
  }

  def onOperationMatcherRequest(currentState: State, header: SPHeader, response: API.Request): Unit = {
    import sp.operationmatcher.API
    if (header.to == APIAbilityHandler.service) {
      log.debug(s"ABH from OP matcher: $response")
      response match {
        case API.Find(pairs: Map[String, SPValue]) =>
          val responseHeader = header.swapToAndFrom()

          publish(APIAbilityHandler.topicResponse, SPMessage.makeJson(responseHeader, APISP.SPACK()))

          val _abilities = currentState.abilities.toSet

          val matchingAbilities = _abilities.filter { ability =>
            pairs.forall(pair => ability.pairs.exists(_ == pair))
          }

          val neighbors = _abilities.filter { ability =>
            pairs.forall { case (k, _) => ability.pairs.contains(k) }
          }.diff(matchingAbilities)

          val matches = API.Matches(matchingAbilities.toList, neighbors.toList)
          val msg = SPMessage.makeJson(responseHeader, matches)

          publish(APIAbilityHandler.topicResponse, msg)
          publish(APIAbilityHandler.topicResponse, SPMessage.makeJson(responseHeader, APISP.SPDone()))
      }
    }
  }

  def onVirtualDeviceRequest(currentState: State, header: SPHeader, response: APIVirtualDevice.Response): Unit = {
    println("VirtualDevice stuff")
    if (header.from.contains(virtualDeviceID.toString) || header.reply == SPValue(handlerID)) {
      log.debug("ABH from VD: " + response)
      response match {
        case stateEvent: APIVirtualDevice.StateEvent =>
          transitionState(currentState.copy(states = currentState.states ++ stateEvent.state))

          // Add filters if we need it later. Probably is better that all abilities has
          // the complete state
          //val f = abilities.filter(kv => kv._2.ids.intersect(s.keySet).nonEmpty)
          //f.foreach{kv => kv._2.actor ! NewState(filterState(kv._2.ids, state))}

          // The no filter version
          currentState.abilityStates.foreach { case (_, abilityState) => abilityState.actor ! NewState(currentState.states) }

        case virtualDevice: APIVirtualDevice.TheVD =>
          println("We got the VD!")
          println(virtualDevice)
          val _resources = virtualDevice.resources.map(_.r)
          val _state = virtualDevice.resources.foldLeft(currentState.states)(_ ++ _.state)

          transitionState(currentState.copy(resources = _resources, states = _state))

          currentState.abilityStates.foreach { case (_, abilityState) =>
            abilityState.actor ! NewState(currentState.states.filterKeys(abilityState.ids.contains))
          }

        case _ => Unit
      }
    }
  }

  implicit class MapUpdate[T](map: Map[ID, T]) {
    /**
      * Applies f to the value at map(key), if the value is present.
      * @return A new map with the updated value if the value was present,
      *         or the old map if the value was not present.
      */
    def updateValue(key: ID)(f: T => T): Map[ID, T] = {
      map.get(key).map(v => map.updated(key, f(v))).getOrElse(map)
    }
  }

  implicit class AbilityKeys(ability: APIAbilityHandler.Ability) {
    /**
      * Data associated with the ability
      */
    def pairs: Map[String, SPValue] = ability.attributes.getAs[Map[String, SPValue]]("pairs").getOrElse(Map())
  }

  implicit class BetterSPMessage(message: SPMessage) {
    def as[T](implicit reads: JSReads[T]): Option[(SPHeader, T)] = for {
      header <- message.getHeaderAs[SPHeader]
      body <- message.getBodyAs[T]
    } yield (header, body)

    def oneOf[A](implicit reads: JSReads[A]): OneOf[SPHeader, A] = OneOf[SPHeader, A](message)

    case class OneOf[H, A](message: SPMessage, prevValues: List[Any] = Nil)(implicit reads: JSReads[A], headerReads: JSReads[H]) {
      val header: Option[H] = message.getHeaderAs[H]
      val values: List[Any] = message.getBodyAs[A] match {
        case Some(v) => v :: prevValues
        case None => prevValues
      }

      def or[B](implicit bReads: JSReads[B]): OneOf[H, B] = OneOf[H, B](message, values)
      def body: Option[Any] = values match {
        case Nil => None
        case h :: _ => Some(h)
      }

      def foreach(f: Any => Unit): Unit = body.foreach(f)
      def map[B](f: Any => B): Option[B] = body.map(f)
      def isEmpty[B](f: Any => B): Boolean = body.isEmpty
      def isDefined[B](f: Any => B): Boolean = body.isDefined
      def nonEmpty[B](f: Any => B): Boolean = body.nonEmpty
    }
  }
}

