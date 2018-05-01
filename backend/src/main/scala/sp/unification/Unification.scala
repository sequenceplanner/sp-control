package sp.unification

import akka.actor._
import sp.abilityhandler.APIAbilityHandler
import sp.devicehandler._
import sp.domain.Logic.{makeStructNodes, _}
import sp.domain._
import sp.models.{APIModel, APIModelMaker}
import sp.runners._
import sp.service.MessageBussSupport

import sp.devicehandler.VD.DriverStateMapper
import sp.domain.logic.{ActionParser, PropositionParser}
import sp.drivers.{HumanDriver, ROSDriver, URDriver}

import scala.concurrent.duration._

class Unification extends Actor with MessageBussSupport{
  import context.dispatcher

  subscribe(APIModel.topicResponse)
  subscribe(APIModelMaker.topicResponse)
  subscribe(APIUnification.topicRequest)

  def receive = {
    case s : String =>

      for { // unpack message
        mess <- SPMessage.fromJson(s)
        h <- mess.getHeaderAs[SPHeader] if  h.to == APIUnification.service
        b <- mess.getBodyAs[APIUnification.Request]
      } yield {
        val spHeader = h.swapToAndFrom
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK())) // acknowledge message received
        b match { // Check if the body is any of the following classes, and execute program
          case APIUnification.sendUnificationModel(name, resources, setupRunner) => saveModel(name, resources, setupRunner)
          case APIUnification.launchVDAbilities(idables) => launchVDAbilities(idables)
          case APIUnification.launchOpRunner(idables) => launchOpRunner(idables)
          case x => // otherwise, nothing
        }
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))
      }
  }
  def sendAnswer(mess: String) = publish(APIUnification.topicResponse, mess)


  def launchVDAbilities(ids : List[IDAble])= {

    // Extract model data from IDAbles
    val ops = ids.filter(_.isInstanceOf[Operation]).map(_.asInstanceOf[Operation])
    val things = ids.filter(_.isInstanceOf[Thing]).map(_.asInstanceOf[Thing])
    val rTmp = things.filter(t => t.attributes.keys.contains("stateMap"))
    val setupRunnerThings = things.filter(t => t.name == "setupRunnerAsThing")

    val exAbilities = ops.map(o=> APIAbilityHandler.operationToAbility(o))
    val exResorces = rTmp.map(t => VD.thingToResource(t))
    val exDrivers = things.diff(rTmp).diff(setupRunnerThings).map(t=> VD.thingToDriver(t))
    val exSetupRunner = APIOperationRunner.CreateRunner(thingToSetup(setupRunnerThings.head))

    //Direct launch of the VD and abilities below
    val vdID = ID.newID
    val abID = ID.newID

    publish(APIVirtualDevice.topicRequest,
      SPMessage.makeJson(
        SPHeader(from = "UnificationAbilities"),
        APIVirtualDevice.SetUpVD(
          name = "UnificationVD",
          id = vdID,
          exResorces,
          exDrivers,
          attributes = SPAttributes()
        )))

    publish(APIAbilityHandler.topicRequest,
      SPMessage.makeJson(
        SPHeader(from = "UnificationAbilities"),
        APIAbilityHandler.SetUpAbilityHandler(
          name = "UnificationAbilites",
          id = abID,
          exAbilities,
          vd = vdID
        )))
  }

  def launchOpRunner(ids : List[IDAble])= {

    // Extract setup data from IDAbles
    val things = ids.filter(_.isInstanceOf[Thing]).map(_.asInstanceOf[Thing])
    val setupRunnerThings = things.filter(t => t.name == "setupRunnerAsThing")

    val exSetupRunner = APIOperationRunner.CreateRunner(thingToSetup(setupRunnerThings.head))

    //direct launch of operation runner
    publish(APIOperationRunner.topicRequest, SPMessage.makeJson(
      SPHeader(from = "UnificationAbilities", to = APIOperationRunner.service), exSetupRunner))
  }


  def saveModel(name : String, resources : List[sp.unification.ResourceAndAbilities], setupRunner : APIOperationRunner.CreateRunner) = {

    val cm = sp.models.APIModelMaker.CreateModel(name, SPAttributes("isa" -> "VD"))

    val ops = resources.flatMap(_.abilities).map(APIAbilityHandler.abilityToOperation)
    val rIDable = resources.map(r => VD.resourceToThing(r.resource))
    val dIDable = resources.map(r => VD.driverToThing(r.driver))
    val vars = resources.map(r=> r.vars).flatten
    val setup = setupToThing(setupRunner.setup)

    val xs = rIDable ++ dIDable ++ vars :+ setup
    val theVD = Struct(
      "TheVD",
      makeStructNodes(dIDable.map(StructWrapper): _*)
        ++ makeStructNodes(vars.map(StructWrapper): _*)
        ++ makeStructNodes(rIDable.map(StructWrapper): _*)
        ++ makeStructNodes(ops.map(StructWrapper): _*)
        ++ makeStructNodes(List(setup).map(StructWrapper): _*),

      SPAttributes("isa" -> "VD")
    )

    val addItems = APIModel.PutItems(theVD :: ops ++ xs, SPAttributes("info" -> "initial items"))

    context.system.scheduler.scheduleOnce(1 seconds) {
      publish(
        APIModelMaker.topicRequest,
        SPMessage.makeJson(SPHeader(from = "UnificationAbilities", to = APIModelMaker.service), cm)
      )
    }

    context.system.scheduler.scheduleOnce(1.1 seconds) {
      publish(
        APIModel.topicRequest,
        SPMessage.makeJson(SPHeader(from = "UnificationAbilities", to = cm.id.toString), addItems)
      )
    }
  }


  def setupToThing(setup : APIOperationRunner.Setup): Thing = {
    Thing(
      name = "setupRunnerAsThing",
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
}

object Unification {
  def props = Props(classOf[Unification])
}
