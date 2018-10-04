package sp.modelSupport

import akka.actor._
import sp.devicehandler._
import sp.domain.Logic._
import sp.domain._
import sp.models.{APIModel, APIModelMaker}
import sp.runners._
import sp.service.MessageBussSupport
import sp.vdtesting.APIVDTracker
import scala.concurrent.duration._


object ModelService {
  def props(models: Map[String, ModelDSL]) = Props(classOf[ModelService], models)
}

class ModelService(models: Map[String, ModelDSL]) extends Actor with MessageBussSupport{
  import context.dispatcher

  def updateVariableIDs(p: Condition, remap: Map[ID, ID]): Condition = {
    p.copy(guard = updateGuard(p.guard, remap), action = p.action.map(a=>updateAction(a,remap)))
  }

  def updateGuard(guard: Proposition, remap: Map[ID, ID]): Proposition = {
    def updateID(pe: StateEvaluator): StateEvaluator = pe match {
      case SVIDEval(id) => SVIDEval(remap.get(id).getOrElse(id))
      case x => x
    }

    guard match {
      case AND(xs) => AND(xs.map(x=>updateGuard(x, remap)))
      case OR(xs) => OR(xs.map(x=>updateGuard(x, remap)))
      case NOT(x) => NOT(updateGuard(x, remap))
      case EQ(left, right) => EQ(updateID(left), updateID(right))
      case NEQ(left, right) => EQ(updateID(left), updateID(right))
      case GREQ(left, right) => EQ(updateID(left), updateID(right))
      case LEEQ(left, right) => EQ(updateID(left), updateID(right))
      case GR(left, right) => EQ(updateID(left), updateID(right))
      case LE(left, right) => EQ(updateID(left), updateID(right))
      case AlwaysTrue => AlwaysTrue
      case AlwaysFalse => AlwaysFalse
    }
  }

  def updateAction(action: Action, remap: Map[ID, ID]): Action = {
    val nid = remap.get(action.id).getOrElse(action.id)
    val nval = action.value match {
      case ASSIGN(id) => ASSIGN(remap.get(id).getOrElse(id))
      case x => x
    }
    action.copy(id = nid, value = nval)
  }


  subscribe(APIModel.topicResponse)
  subscribe(APIModelMaker.topicResponse)
  subscribe(APIVDTracker.topicRequest)

  def launchVDAbilities(ids : List[IDAble]): Unit= {
    // Extract model data from IDAbles
    val operations = ids.collect { case op: Operation => op }
    val things = ids.collect { case thing: Thing => thing }

    val setupRunnerThings = things.filter(_.attributes.keys.contains("runnerID"))

    val resourceThings = things.filter(_.attributes.keys.contains("stateMap"))
    val resources = resourceThings.map(VD.thingToResource)

    val driverThings = things.filter(_.attributes.keys.contains("driverType"))
    val drivers = driverThings.map(VD.thingToDriver)

    //Direct launch of the VD and abilities below
    val (virtualDeviceId, abilityId) = (ID.newID, ID.newID)


    // Merge abilities and ops
    // TODO: do this in model building
    val rt = setupRunnerThings.head
    val runnerSetup = APIOperationRunner.runnerThingToSetup(rt)
    val opAbMap = runnerSetup.opAbilityMap
    val vToDvMap = runnerSetup.variableMap

    // logical variables
    val vs = things.filterNot(t => vToDvMap.keySet.contains(t.id))
    val initialState = vs.flatMap(t => t.attributes.get("init").map(v=>t.id->v)).toMap

    // driver variables
    val dvs = things.filter(t => vToDvMap.values.toList.contains(t.id))

    val ops = operations.filterNot(_.attributes.getAs[String]("isa") == Some("Ability"))
    val abs = operations.filter(_.attributes.getAs[String]("isa") == Some("Ability"))
    val abOps = for {
      o <- ops
      aid <- opAbMap.get(o.id)
      a <- abs.find(_.id==aid)
    } yield {
      o.copy(conditions = o.conditions.map(c => updateVariableIDs(c, vToDvMap)) ++ a.conditions)
    }
    val mergedOps = abOps ++ ops.filterNot(o=>abOps.exists(oa=>oa.id==o.id))


    publish(APIVirtualDevice.topicRequest,
      SPMessage.makeJson(
        SPHeader(from = "ModelService"),
        APIVirtualDevice.SetUpVD2(
          name = "VD",
          id = virtualDeviceId,
          mergedOps, // TEMP send on abilities here...
          resources, //= resources.map(_.resource),
          drivers, // = resources.map(_.driver),
          initialState,
          attributes = SPAttributes()
        )
      )
    )
  }

  def launchOpRunner(h: SPHeader, ids : List[IDAble])= {

    // Extract setup data from IDAbles
    val things = ids.collect { case thing: Thing => thing }
    val setupRunnerThings = things.filter(_.attributes.keys.contains("runnerID"))

    println("CREATING RUNNERS" + setupRunnerThings)


    setupRunnerThings.foreach { thing =>
      println("STARTING RUNNER" + thing.toString)
      val runnerSetup = APIOperationRunner.runnerThingToSetup(thing).copy(runnerID = ID.newID)
      val exSetupRunner = APIOperationRunner.CreateRunner(runnerSetup)

      //println("RUNNER SETUP: " + exSetupRunner.toString)

      publish(
        APIOperationRunner.topicRequest,
        SPMessage.makeJson(
          SPHeader(from = "ModelService", to = APIOperationRunner.service),
          exSetupRunner
        )
      )

      println("RUNNER STARTED")

      publish(APIVDTracker.topicResponse, SPMessage.makeJson(h, APIVDTracker.OpRunnerCreated(runnerSetup.runnerID)))

      println("SENT TO FRONTEND")
    }

  }

  def createModel(name: String, modelID: ID): Unit = {
    //val modelID = ID.makeID("0d80d1d6-48cd-48ec-bfb1-d69714ef35be").get // hardcoded model id so we do not get a new model every time

    val model = models(name)

    val idables = model.buildModel()
    val attributes = SPAttributes("isa" -> "VD")

    val newModel = sp.models.APIModelMaker.CreateModel(name, attributes, id = modelID)

    val virtualDevice = Struct(name, makeStructNodes(idables), attributes)

    val items = APIModel.PutItems(virtualDevice :: idables, SPAttributes("info" -> "initial items"))

    context.system.scheduler.scheduleOnce(0.1 seconds) {
      publish(
        APIModelMaker.topicRequest,
        SPMessage.makeJson(SPHeader(from = "ModelService", to = APIModelMaker.service), newModel)
      )
    }

    context.system.scheduler.scheduleOnce(1 seconds) {
      publish(
        APIModel.topicRequest,
        SPMessage.makeJson(SPHeader(from = "ModelService", to = newModel.id.toString), items)
      )
    }
  }

  def setupToThing(setup : APIOperationRunner.Setup): Thing = {
    Thing(
      name = setup.name,
      id = ID.newID,
      attributes = SPAttributes(
        "name" -> setup.name,
        "runnerID" -> setup.runnerID,
        "ops" -> setup.ops,
        "opAbilityMap" -> setup.opAbilityMap,
        "initialState" -> setup.initialState,
        "variableMap" -> setup.variableMap,
        "abilityParameters" -> setup.abilityParameters.toList
      )
    )
  }

  def runnerSetupFor(thing: Thing): APIOperationRunner.Setup = {
    val attrs = thing.attributes

    val name = attrs.getWithDefault("name", "")
    val runnerID = attrs.getWithDefault("runnerID", ID.newID)
    val ops = attrs.getWithDefault("ops", Set[Operation]())
    val opAbilityMap = attrs.getWithDefault("opAbilityMap", Map[ID, ID]())
    val initialState = attrs.getWithDefault("initialState", Map[ID, SPValue]())
    val variableMap = attrs.getWithDefault("variableMap", Map[ID, ID]())
    val abilityParameters = attrs.getWithDefault("abilityParameters", List[(ID,Set[ID])]()).toMap

    APIOperationRunner.Setup(name, runnerID, ops, opAbilityMap, initialState, variableMap, abilityParameters)
  }

  def receive: Receive = {
    case s : String =>
      for { // unpack message
        message <- SPMessage.fromJson(s)
        header <- message.getHeaderAs[SPHeader] if  header.to == APIVDTracker.service
        body <- message.getBodyAs[APIVDTracker.Request]
      } yield {
        val responseHeader = header.swapToAndFrom()
        sendAnswer(SPMessage.makeJson(responseHeader, APISP.SPACK())) // acknowledge message received

        body match { // Check if the body is any of the following classes, and execute program
          case APIVDTracker.createModel(modelName, modelID) =>
            createModel(modelName, modelID)

          case APIVDTracker.launchVDAbilities(idAbles) =>
            launchVDAbilities(idAbles)

          case APIVDTracker.launchOpRunner(idAbles) =>
            launchOpRunner(responseHeader,idAbles)

          case APIVDTracker.getModelsInfo(_) =>
            sendAnswer(SPMessage.makeJson(responseHeader, APIVDTracker.sendModelsInfo(models.keys.toList)))

          case _ => Unit
        }
        sendAnswer(SPMessage.makeJson(responseHeader, APISP.SPDone()))
      }
  }
  def sendAnswer(mess: String): Unit = publish(APIVDTracker.topicResponse, mess)

  implicit class EnhancedSPAttributes(attributes: SPAttributes) {
    def getWithDefault[A](key: String, default: => A)(implicit reads: JSReads[A]): A = attributes.getAs[A](key).getOrElse(default)
  }
}
