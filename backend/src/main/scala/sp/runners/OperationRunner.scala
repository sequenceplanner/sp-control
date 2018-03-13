package sp.runners

import akka.actor._

import scala.util.{Failure, Random, Success, Try}
import sp.domain._
import sp.domain.Logic._
import sp.runners.APIOperationRunner.ForceComplete
import sp.runners.{APIOperationRunner => api}
import sp.abilityhandler.{APIAbilityHandler => abilityAPI}


class OperationRunner extends Actor
  with ActorLogging
  with OperationRunnerLogic
  with sp.service.ServiceCommunicationSupport
  with sp.service.MessageBussSupport {

  import context.dispatcher
  subscribe(APIOperationRunner.topicRequest)
  subscribe(abilityAPI.topicResponse)
  subscribe(sp.devicehandler.APIVirtualDevice.topicResponse)

  // Setting up the status response that is used for identifying the service in the cluster
  val statusResponse = OperationRunnerInfo.attributes.copy(
    instanceName = "OperationRunner"
  )



  val myH = SPHeader(from = api.service, to = abilityAPI.service, reply = api.service)



  def receive = {
    case x: String if sender() != self =>
      val mess = SPMessage.fromJson(x)

      log.debug("OP RUNNER.........................")
      log.debug(mess.toString)

      matchRequests(mess)
      matchAbilityAPI(mess)
      matchVDAPI(mess)
      // if needed, also get the state from the VD here


  }



  def matchRequests(mess: Option[SPMessage]) = {
    OperationRunnerComm.extractRequest(mess).foreach{ case (h, b) =>
      val updH = h.copy(from = api.service)
      publish(APIOperationRunner.topicResponse, OperationRunnerComm.makeMess(updH, APISP.SPACK()))
      publish(abilityAPI.topicRequest, OperationRunnerComm.makeMess(myH, abilityAPI.GetAbilities))
      b match {
        case api.CreateRunner(setup) =>
          addRunner(setup).foreach{xs =>
            publish(APIOperationRunner.topicResponse, OperationRunnerComm.makeMess(updH, api.Runners(xs)))

            val state = runners(setup.runnerID).currentState

            log.debug("Runner started. Init state: " + state)
            setRunnerState(setup.runnerID, SPState(state = state), startAbility, sendState(_, setup.runnerID), false)

          }

        case api.SetState(id, s) =>
          if (runners.contains(id))
            setRunnerState(id, SPState(state = s), startAbility, sendState(_, id))
            else
              publish(APIOperationRunner.topicResponse, OperationRunnerComm.makeMess(updH, APISP.SPError(s"no runner with id: $id")))

        case api.AddOperations(id, ops, map) =>
          updRunner(id, ops, Set(), map, startAbility, sendState(_, id) )

        case api.RemoveOperations(id, ops) =>
          updRunner(id, Set(), ops, Map(), startAbility, sendState(_, id) )

        case api.TerminateRunner(id) =>
          val xs = removeRunner(id)
          publish(APIOperationRunner.topicResponse, OperationRunnerComm.makeMess(updH, api.Runners(xs)))

        case api.GetState(id) =>
          getRunnerState(id) match {
            case Some(s) =>
              publish(api.topicResponse, OperationRunnerComm.makeMess(updH, api.StateEvent(id, s)))
            case None =>
              publish(APIOperationRunner.topicResponse, OperationRunnerComm.makeMess(updH, APISP.SPError(s"no runner with id: $id")))
          }

        case api.GetRunners =>
          val xs = runners.map(_._2.setup).toList
          publish(APIOperationRunner.topicResponse, OperationRunnerComm.makeMess(updH, api.Runners(xs)))

        case api.ForceComplete(id) =>
          newAbilityState(id, sp.abilityhandler.AbilityState.finished, startAbility, sendState)
       }

      publish(APIOperationRunner.topicResponse, SPMessage.makeJson(updH, APISP.SPDone()))


    }
  }


  def matchAbilityAPI(mess: Option[SPMessage]) = {
    OperationRunnerComm.extractAbilityReply(mess).foreach { case (h, b) =>

        b match {

          case abilityAPI.AbilityStarted(id) =>
            val ops = getOPFromAbility(id).flatMap(_._2)
            log.debug(s"The ability with id $id started for operations: $ops")

          case abilityAPI.AbilityCompleted(id, _) =>
            val ops = getOPFromAbility(id).flatMap(_._2)
            log.debug(s"The ability with id $id completed for operations: $ops")

            newAbilityState(id, sp.abilityhandler.AbilityState.finished, startAbility, sendState)

          case abilityAPI.AbilityState(id, s) =>
            //val ops = getOPFromAbility(id).flatMap(_._2)
            val abState = (for {
              x <- s.get(id) if x.isInstanceOf[SPAttributes]
              v <- x.asInstanceOf[SPAttributes].get("state")
            } yield v).getOrElse(SPValue("notEnabled"))
            log.debug(s"The ability with id $id updated with state: $abState")
            newAbilityState(id, abState, startAbility, sendState)

          case x => log.debug(s"Operation Runner got a message it do not handle: $x")
        }
    }
  }

  def matchVDAPI(mess: Option[SPMessage]) = {
    for {
      m <- mess
      h <- m.getHeaderAs[SPHeader]
      b <- m.getBodyAs[sp.devicehandler.APIVirtualDevice.Response]
    } yield {
      b match {
        case x: sp.devicehandler.APIVirtualDevice.StateEvent =>
          log.debug("we got a VD state: ")
          x.state.foreach(kv => log.debug(kv.toString))
          newResourceState(x.state, startAbility, sendState)
        case x: sp.devicehandler.APIVirtualDevice.TheVD =>
          val state = x.resources.flatMap(_.state).toMap
          newResourceState(state, startAbility, sendState)
      }



    }
  }



  val startAbility: (ID, Map[ID, SPValue]) => Unit = (id: ID, params: Map[ID, SPValue]) => {
    log.debug("Starting ability: " + id)
    val myH = SPHeader(from = api.service, to = abilityAPI.service, reply = api.service)
    publish(abilityAPI.topicRequest, OperationRunnerComm.makeMess(myH, abilityAPI.StartAbility(id, params)))
  }

  val sendState = (s: SPState, id: ID) => {
    log.debug("")
    log.debug(s"new state for $id: ")
    s.state.foreach(x => log.debug(x.toString))
    val myH = SPHeader(from = api.service)
    publish(api.topicResponse, OperationRunnerComm.makeMess(myH, api.StateEvent(id, s.state)))

  }




}

object OperationRunner {
  def props = Props(classOf[OperationRunner])
}






/*
 * The logic for running the operations
 *
 * TODO: 180308: maybe update to use the OperationLogic in sp-domain and OperationStateDefinition
 */
trait OperationRunnerLogic {
  def log: akka.event.LoggingAdapter


  case class Runner(setup: api.Setup, currentState: Map[ID, SPValue]) {
    val noAbilityOps = setup.ops.filter(o => !setup.opAbilityMap.contains(o.id))
  }
  var runners: Map[ID, Runner] = Map()
  var abilities: Set[ID] = Set()


  object OperationState {
    val init: SPValue = "i"
    val executing: SPValue = "e"
    val finished: SPValue = "f"
    val domain = Set(init, executing, finished)
  }


  def updOPs(o: Operation, opAbilityMap: Map[ID, ID]) = {
    opAbilityMap.get(o.id).map{id =>
      val c = Condition(EQ(SVIDEval(id), ValueHolder("enabled")), List(), SPAttributes("kind" -> "pre"))
      o.copy(conditions = c :: o.conditions)
    }.getOrElse(o)
  }

  def addRunner(setup: api.Setup) = {
    val updState = setup.initialState ++
      setup.ops.map(o => o.id -> SPValue(OperationState.init)) ++
      setup.opAbilityMap.values.toList.map(id => id -> SPValue("notEnabled"))
    val updOps = setup.ops.map(o => updOPs(o, setup.opAbilityMap))
    val r = Runner(setup.copy(initialState = updState, ops = updOps), updState)
    if (! validateRunner(setup)) None
    else {
      runners += setup.runnerID -> r
      Some(runners.values.toList.map(_.setup))
    }
  }

  def updRunner(runner: ID,
                add: Set[Operation],
                remove: Set[ID],
                opAbilityMap: Map[ID, ID],
                startAbility: (ID, Map[ID, SPValue]) => Unit,
                sendState: SPState => Unit
               ) = {
    val updR = runners.get(runner).map {runner =>
      val updMap = (runner.setup.opAbilityMap ++ opAbilityMap).filter(kv => !remove.contains(kv._1))
      val updOps = (runner.setup.ops ++ add).filter(o => !remove.contains(o.id)).map(o => updOPs(o, updMap))
      val updSetup = runner.setup.copy(ops = updOps, opAbilityMap = updMap)
      val updState = (runner.currentState ++ add.map(o => o.id -> SPValue(OperationState.init))).filter(kv => !remove.contains(kv._1)) ++
        updMap.values.toList.map(id => id -> SPValue("notEnabled"))

      Runner(updSetup, updState)
    }
    updR.foreach{r =>
      runners += runner -> r
      setRunnerState(runner, SPState(state = r.currentState), startAbility, sendState)
    }
  }

  private def validateRunner(setup: api.Setup) = {
    !runners.contains(setup.runnerID)
  }

  def newAbilityState(ability: ID, abilityState: SPValue, startAbility: (ID, Map[ID, SPValue]) => Unit, sendState: (SPState, ID) => Unit): Unit = {
    runners.foreach{r =>
      if (r._2.setup.opAbilityMap.values.toSet.contains(ability)){
        log.debug("An ability has an operation and was updated")
        val cS = SPState(state = runners(r._1).currentState + (ability -> abilityState))
        setRunnerState(r._1, cS, startAbility, sendState(_,r._1))
      }
    }
  }

  def newResourceState(state: Map[ID, SPValue], startAbility: (ID, Map[ID, SPValue]) => Unit, sendState: (SPState, ID) => Unit, runOneAtTheTime: Boolean = false) = {
    runners.foreach{r =>
      val reMap = r._2.setup.variableMap.map(kv => kv._2 -> kv._1)
      val myThings = state.filter(kv => reMap.contains(kv._1))
      if (myThings.nonEmpty){
        log.debug("A resource state with connected variables have been updated")
        val remapState = myThings.map(kv => reMap(kv._1) -> kv._2)
        val cS = SPState(state = runners(r._1).currentState ++ remapState)
        setRunnerState(r._1, cS, startAbility, sendState(_,r._1), runOneAtTheTime)
      }
    }
  }

  def setRunnerState(runnerID: ID, s: SPState, startAbility: (ID, Map[ID, SPValue]) => Unit, sendState: SPState => Unit, runOneAtTheTime: Boolean = false): Unit = {
    val r = runners.get(runnerID)
    r.foreach { x =>
      val theS = x.currentState ++ s.state
      val theState = SPState(state = theS)
      log.debug("set runner state from: " + x.currentState + " to " + theS)
      if (theS != x.currentState) sendState(theState)
      val updS = newState(theState, x.setup.ops, x, startAbility, sendState, runOneAtTheTime)
      runners += runnerID -> x.copy(currentState = updS.state)
    }
  }

  def tickRunner(runnerID: ID, startAbility: (ID, Map[ID, SPValue]) => Unit, sendState: SPState => Unit, runOneAtTheTime: Boolean = false): Unit = {
    runners.get(runnerID).foreach { x =>
      val theState = SPState(state = x.currentState)
      val updS = newState(theState, x.setup.ops, x, startAbility, sendState, runOneAtTheTime)
      runners += runnerID -> x.copy(currentState = updS.state)
    }
  }

  def removeRunner(id: ID) = {
    runners = runners - id
    runners.values.toList.map(_.setup)
  }

  def getRunnerState(id: ID) = {
    runners.get(id).map(_.currentState)
  }



  import scala.annotation.tailrec


  /**
    * The main method that updates the runs the operations, by executing the various conditions
    * It also starts abilities and and send out state changes (to upper level system and to virtual devices)
    *
    * @param s Current state
    * @param ops The operaitons to evaluate
    * @param r The runner, since we need to know the mapping to abilities
    * @param startAbility A function from the actor side for starting the abilities
    * @param sendState A function from the actor side to send the state of the runner
    * @param runOneAtTheTime A flag to use if only one operation should start at the time.
    *                        If false, every operation that can will start (or end or reset).
    *                        If true, the first operation will start and then the next one will start at the next
    *                        state change
    * @return The updated state
    */
  @tailrec final def newState(s: SPState, ops: Set[Operation], r: Runner, startAbility: (ID, Map[ID, SPValue]) => Unit, sendState: SPState => Unit, runOneAtTheTime: Boolean = false): SPState = {

    val filterOps = ops.foldLeft((List[Operation](), List[Operation](), List[Operation]())){case (aggr, o) =>
      if (isEnabled(o, s)) {
        (aggr._1 :+ o, aggr._2, aggr._3)
      } else if (canComplete(o, s, r.setup.opAbilityMap)) {
        (aggr._1, aggr._2  :+ o, aggr._3)
      } else if (canReset(o, s)){
        (aggr._1, aggr._2, aggr._3 :+ o)
      } else aggr
    }

    val enabled = filterOps._1
    val complete = filterOps._2
    val reset = filterOps._3

    var opsToGo = ops

    val resRes = reset.headOption.map{o =>
      opsToGo -= o
      val updS = resetOP(o, s)
      sendState(updS)
      updS
    }.getOrElse(s)

    val resCompl = complete.headOption.map{o =>
      opsToGo -= o
      val updS = completeOP(o, resRes)
      sendState(updS)
      updS
    }.getOrElse(resRes)

    val res = enabled.headOption.map{o =>
      opsToGo -= o
      val updS = runOp(o, resCompl)
      sendState(updS)

      // TODO: Use another mechanism to send parameters. Maybe use names or a trait with predefined parameter names. It should be up to the ability to map the parameters into the resource state...
      r.setup.opAbilityMap.get(o.id).foreach(id => startAbility(id, prepareAbilityParameters(id, r, updS.state)))
      updS
    }.getOrElse(resCompl)



    if (enabled.nonEmpty && complete.nonEmpty && reset.nonEmpty)log.info("vv*************")
    if (enabled.isEmpty && complete.isEmpty && reset.isEmpty) log.debug("runner no ops changing: ")
    if (enabled.nonEmpty)  log.info("runner enabled ops: " + enabled.map(_.name).mkString(", "))
    if (complete.nonEmpty) log.info("runner can complete ops: " + complete.map(_.name).mkString(", "))
    if (reset.nonEmpty)    log.info("runner can reset ops: " + reset.map(_.name).mkString(", "))
    if (enabled.nonEmpty && complete.nonEmpty && reset.nonEmpty) log.info(res.toString)
    if (enabled.nonEmpty && complete.nonEmpty && reset.nonEmpty)log.info("*************")


    if (runOneAtTheTime || res == s)
      res
    else
      newState(res, opsToGo, r, startAbility, sendState, runOneAtTheTime)

  }

  def prepareAbilityParameters(ability: ID, r: Runner, state: Map[ID, SPValue]) = {
   for {
     kv <- state if r.setup.variableMap.contains(kv._1)
     xs <- r.setup.abilityParameters.get(ability) if xs.contains(kv._1)
   } yield {
     r.setup.variableMap(kv._1) -> kv._2
   }
  }


  def runOp(o: Operation, s: SPState) = {
      val filtered = filterConditions(o.conditions, Set("pre", "precondition"))
      val newState = filtered.foldLeft(s){(tempS, cond) => cond.next(tempS)}
      log.debug(s"${o.name} started")
      newState.next(o.id -> OperationState.executing)
  }

  def completeOP(o: Operation, s: SPState) = {
    val filtered = filterConditions(o.conditions, Set("post", "postcondition"))
    val newState = filtered.foldLeft(s){(tempS, cond) => cond.next(tempS)}
    log.debug(s"${o.name} completed")
    newState.next(o.id -> OperationState.finished)
  }

  def resetOP(o: Operation, s: SPState) = {
    val filtered = filterConditions(o.conditions, Set("reset", "resetcondition"))
    val newState = filtered.foldLeft(s){(tempS, cond) => cond.next(tempS)}
    log.debug(s"${o.name} reset")
    newState.next(o.id -> OperationState.init)
  }

  def evaluateOps(ops: List[Operation], s: SPState) = {
    ops.filter(o => isEnabled(o, s))
  }

  def isEnabled(o: Operation, s: SPState): Boolean = {
    (s(o.id) == OperationState.init) &&
      filterConditions(o.conditions, Set("pre", "precondition")).forall(_.eval(s)) // Always true if no precond

  }

  def canComplete(o: Operation, s: SPState, opAbilityMap: Map[ID, ID]): Boolean = {
    s(o.id) == OperationState.executing &&
      ((!opAbilityMap.contains(o.id) && filterConditions(o.conditions, Set("post", "postcondition")).forall(_.eval(s))) || // always true if no postcond
        s.get(opAbilityMap(o.id)).contains(SPValue(sp.abilityhandler.AbilityState.finished)))


  }

  def canReset(o: Operation, s: SPState): Boolean = {
    (s(o.id) == OperationState.finished) && {
      val xs = filterConditions(o.conditions, Set("reset", "resetcondition"))
      xs.forall(_.eval(s)) && xs.nonEmpty // always false if no resetcond
    }
  }


  def filterConditions(conds: List[Condition], set: Set[String]) = {
    conds filter(c => {
      val res = c.attributes.getAs[String]("kind").getOrElse("")
      (set contains res) || set.isEmpty
    })
  }

  def getOPFromAbility(id: ID) = {
    runners.filter{r =>
      r._2.setup.opAbilityMap.values.toSet.contains(id) // maybe precalculate this
    }.map(kv => kv._1 -> kv._2.setup.opAbilityMap.foldLeft(Set[ID]())((a, b) => {
      if (b._2 == id) a + b._1 else a
    })).toList

  }


}
