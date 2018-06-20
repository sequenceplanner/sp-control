package sp.abilityhandler

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import sp.devicehandler.{APIVirtualDevice, VD}
import sp.domain.APISP.StatusResponse
import sp.domain._
import sp.domain.Logic._
import sp.operationmatcher.API
import sp.operationmatcher.{API => omapi}

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
          publish(APIAbilityHandler.topicResponse, makeMess(updH, APIAbilityHandler.AbilitiesByIdAndName(abs)))

          publish(APIAbilityHandler.topicResponse, makeMess(updH, APISP.SPDone()))

          abilities.foreach(a => a._2.actor ! GetState)

        case APIAbilityHandler.GetAbility(id) =>
          // Get a list of all abilities
          val listOfAbilities = abilities.map(_._2.ability).toList
          // Try to find the ability in the list that match with id
          val matchedAbilityInList = listOfAbilities.find{ ability => ability.id == id }
          // make message with header and trigger the response with The Ability
          publish(APIAbilityHandler.topicResponse, makeMess(updH, APIAbilityHandler.TheAbility(matchedAbilityInList)))


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

        case other =>
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