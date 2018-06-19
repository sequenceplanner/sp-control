package sp.modelSupport

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
  def props(models: Map[String, ModelDSL]) = Props(classOf[ModelService], models)
}

class ModelService(models: Map[String, ModelDSL]) extends Actor with MessageBussSupport{
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

    val exSetupRunner = setupRunnerThings.headOption.map{h=>
      val setup = APIOperationRunner.runnerThingToSetup(h)
      APIOperationRunner.CreateRunner(setup)
    }

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
      val runnerSetup = APIOperationRunner.runnerThingToSetup(s).copy(runnerID = ID.newID)
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

    val name = modelType
    val model = models(modelType)

    val idables = model.buildModel()

    val cm = sp.models.APIModelMaker.CreateModel(name, SPAttributes("isa" -> "VD"), id = modelID)

    val theVD = Struct(
      name,
      makeStructNodes(idables),
      SPAttributes("isa" -> "VD")
    )
    val addItems = APIModel.PutItems(theVD :: idables.toList, SPAttributes("info" -> "initial items"))

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

  def receive = {
    case s : String =>
      for { // unpack message
        mess <- SPMessage.fromJson(s)
        h <- mess.getHeaderAs[SPHeader] if  h.to == APIVDTracker.service
        b <- mess.getBodyAs[APIVDTracker.Request]
      } yield {
        val spHeader = h.swapToAndFrom()
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