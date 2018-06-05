package sp.modelService

import akka.actor._
import sp.abilityhandler.APIAbilityHandler
import sp.devicehandler.VD.DriverStateMapper
import sp.devicehandler._
import sp.domain.Logic._
import sp.domain._
import sp.drivers.ROSFlatStateDriver
import sp.models.{APIModel, APIModelMaker}
import sp.runners._
import sp.service.MessageBussSupport
import sp.vdtesting.APIVDTracker
import sp.abilityhandler.APIAbilityHandler.Ability
import scala.concurrent.duration._


object ModelService {
  def props(models: Map[String, VDHelper]) = Props(classOf[ModelService], models)
}

class ModelService(models: Map[String, VDHelper]) extends Actor with MessageBussSupport{
  import context.dispatcher

  subscribe(APIModel.topicResponse)
  subscribe(APIModelMaker.topicResponse)
  subscribe(APIVDTracker.topicRequest)

  def launchVDAbilities(ids : List[IDAble])= {

    // Extract model data from IDAbles
    val ops = ids.filter(_.isInstanceOf[Operation]).map(_.asInstanceOf[Operation])
    val things = ids.filter(_.isInstanceOf[Thing]).map(_.asInstanceOf[Thing])
    val setupRunnerThings = things.filter(t => t.attributes.keys.contains("runnerID"))
    val exAbilities = ops.flatMap(o=> APIAbilityHandler.operationToAbility(o))

    val rTmp = things.filter(t => t.attributes.keys.contains("stateMap"))
    val exResorces = rTmp.map(t => VD.thingToResource(t))

    val dTmp = things.filter(t => t.attributes.keys.contains("driverType"))
    val exDrivers = dTmp.map(t=> VD.thingToDriver(t))

    val exSetupRunner = setupRunnerThings.headOption.map(h=>APIOperationRunner.CreateRunner(thingToSetup(h)))

    //Direct launch of the VD and abilities below
    val vdID = ID.newID
    val abID = ID.newID

    publish(APIVirtualDevice.topicRequest,
      SPMessage.makeJson(
        SPHeader(from = "ModelService"),
        APIVirtualDevice.SetUpVD(
          name = "VD",
          id = vdID,
          exResorces, //= resources.map(_.resource),
          exDrivers, // = resources.map(_.driver),
          attributes = SPAttributes()
        )))

    publish(APIAbilityHandler.topicRequest,
      SPMessage.makeJson(
        SPHeader(from = "ModelService"),
        APIAbilityHandler.SetUpAbilityHandler(
          name = "Abilites",
          id = abID,
          exAbilities,
          vd = vdID
        )))
  }

  def launchOpRunner(h: SPHeader, ids : List[IDAble])= {

    // Extract setup data from IDAbles

    val things = ids.filter(_.isInstanceOf[Thing]).map(_.asInstanceOf[Thing])
    val setupRunnerThings = things.filter(t => t.attributes.keys.contains("runnerID"))

    println("CREATING RUNNERS" + setupRunnerThings)


    setupRunnerThings.map{s =>
      println("STARTING RUNNER" + s.toString)
      val runnerSetup = thingToSetup(s).copy(runnerID = ID.newID)
      val exSetupRunner = APIOperationRunner.CreateRunner(runnerSetup)

      println("RUNNER SETUP: " + exSetupRunner.toString)

      publish(APIOperationRunner.topicRequest, SPMessage.makeJson(
        SPHeader(from = "ModelService", to = APIOperationRunner.service), exSetupRunner))

      println("RUNNER STARTED")

      publish(APIVDTracker.topicResponse, SPMessage.makeJson(h, APIVDTracker.OpRunnerCreated(runnerSetup.runnerID)))

      println("SENT TO FRONTEND")
    }

  }

  def createModel(modelType: String, modelID: ID) = {
    //val modelID = ID.makeID("0d80d1d6-48cd-48ec-bfb1-d69714ef35be").get // hardcoded model id so we do not get a new model every time

    val model = models(modelType)

    val cm = sp.models.APIModelMaker.CreateModel(model.name, SPAttributes("isa" -> "VD"), id = modelID)

    val abs = model.abilities.values.map(a=>APIAbilityHandler.abilityToOperation(a))
    val vars = model.dthings.values
    val opvars = model.things.values
    val ops = model.operations.values
    val rIDables = model.resources.values.map(r=>VD.resourceToThing(r))
    val dIDables = model.drivers.values.map(d=>VD.driverToThing(d))
    val runnerSetups = model.runners.values.map(setupToThing)

    val all = rIDables ++ dIDables ++ abs ++ vars ++ opvars ++ ops ++ runnerSetups

    val theVD = Struct(
      "TheVD",
      makeStructNodes(all.toList),
      SPAttributes("isa" -> "VD")
    )
    val addItems = APIModel.PutItems(theVD :: all.toList, SPAttributes("info" -> "initial items"))

    context.system.scheduler.scheduleOnce(0.1 seconds) {
      publish(
        APIModelMaker.topicRequest,
        SPMessage.makeJson(SPHeader(from = "ModelService", to = APIModelMaker.service), cm)
      )
    }

    context.system.scheduler.scheduleOnce(0.2 seconds) {
      publish(
        APIModel.topicRequest,
        SPMessage.makeJson(SPHeader(from = "ModelService", to = cm.id.toString), addItems)
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

  def thingToSetup(thing : Thing): APIOperationRunner.Setup = {
    val name = thing.attributes.getAs[String]("name").getOrElse("")
    val runnerID = thing.attributes.getAs[ID]("runnerID").getOrElse(ID.newID)
    val ops = thing.attributes.getAs[Set[Operation]]("ops").getOrElse(Set())
    val opAbilityMap = thing.attributes.getAs[Map[ID,ID]]("opAbilityMap").getOrElse(Map())
    val initialState = thing.attributes.getAs[Map[ID,SPValue]]("initialState").getOrElse(Map())
    val variableMap = thing.attributes.getAs[Map[ID,ID]]("variableMap").getOrElse(Map())
    val abilityParameters = thing.attributes.getAs[List[(ID,Set[ID])]]("abilityParameters").getOrElse(List()).toMap
    APIOperationRunner.Setup(name, runnerID,ops,opAbilityMap,initialState,variableMap,abilityParameters)
  }

  def receive = {
    case s : String =>
      for { // unpack message
        mess <- SPMessage.fromJson(s)
        h <- mess.getHeaderAs[SPHeader] if  h.to == APIVDTracker.service
        b <- mess.getBodyAs[APIVDTracker.Request]
      } yield {
        val spHeader = h.swapToAndFrom
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK())) // acknowledge message received
        b match { // Check if the body is any of the following classes, and execute program
          case APIVDTracker.createModel(modelName, modelID) => createModel(modelName,modelID)
          case APIVDTracker.launchVDAbilities(idables) => launchVDAbilities(idables)
          case APIVDTracker.launchOpRunner(idables) => launchOpRunner(spHeader,idables)
          case APIVDTracker.getModelsInfo(dummy) =>
            sendAnswer(SPMessage.makeJson(spHeader, APIVDTracker.sendModelsInfo(sp.Launch.models.keys.toList)))
          case x =>
        }
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPDone()))
      }
  }
  def sendAnswer(mess: String) = publish(APIVDTracker.topicResponse, mess)

}
