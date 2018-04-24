package sp.volvosim

import akka.actor._
import sp.abilityhandler.APIAbilityHandler
import sp.devicehandler.VD.OneToOneMapper
import sp.devicehandler._
import sp.domain.Logic._
import sp.domain._
import sp.domain.logic.{ActionParser, PropositionParser}
import sp.models.{APIModel, APIModelMaker}
import sp.runners._
import sp.service.MessageBussSupport
import sp.vdtesting.APIVDTracker

import scala.concurrent.duration._


object VolvoSimModel {
  def props = Props(classOf[VolvoSimModel])

  // Jag skapar en enkel modell här som demo.
  // a robot
  val currentProgram = Thing("currentProgram")
  val currentTime = Thing("currentTime")
  val eStop = Thing("eStop")
  val homePosition = Thing("homePosition")

  val theThings: List[Thing] = List(
    currentProgram,
    currentTime,
    eStop,
    homePosition
  )
  val ids: Set[ID] = theThings.map(_.id).toSet

  val driver = VD.Driver("volvoSimulatedRobotDriver", ID.newID, DummyVolvoRobotDriver.driverType, SPAttributes())

  val driverResourceMapper = theThings.map(t =>
    // Must have the same name as the state
    OneToOneMapper(t.id, driver.id, t.name)
  )

  val resource = VD.Resource("VolvoSimulatedRobot", ID.newID, ids, driverResourceMapper, SPAttributes())


  // The ability hardcoded prog (this is so we can test by running abilities).
  // Change so the ability expect the prog name later
  val command = APIAbilityHandler.Ability(
    name = s"robot.prog",
    parameters = List(),
    preCondition = makeCondition(
      "pre",
      "currentTime == 0",
      "currentProgram := prog10")(theThings),
    started = makeCondition("started",s"currentTime != 0")(theThings),
    postCondition = makeCondition("post", "currentTime == 0")(theThings),
    resetCondition = makeCondition("reset", "true")(theThings)
  )




  def makeCondition(kind: String, guard: String, actions: String*)(aList: List[IDAble]) = {
    val g = if (guard == "true") Some(AlwaysTrue)
    else if (guard == "false") Some(AlwaysFalse)
    else PropositionParser(aList).parseStr(guard) match {
      case Right(p) => Some(p)
      case Left(err) => println(s"Parsing failed on condition: $guard: $err"); None
    }
    val xs = actions.flatMap { action =>
      ActionParser(aList).parseStr(action) match {
        case Right(a) => Some(a)
        case Left(err) => println(s"Parsing failed on action: $action: $err"); None
      }
    }
    Condition(g.get, xs.toList, attributes = SPAttributes("kind"->kind))
  }

}


class VolvoSimModel extends Actor with MessageBussSupport{
  import context.dispatcher

  subscribe(APIModel.topicResponse)
  subscribe(APIModelMaker.topicResponse)
  subscribe(APIVDTracker.topicRequest)



  def launchVDAbilities(ids : List[IDAble])= {

    // Extract model data from IDAbles
    val ops = ids.filter(_.isInstanceOf[Operation]).map(_.asInstanceOf[Operation])
    val things = ids.filter(_.isInstanceOf[Thing]).map(_.asInstanceOf[Thing])
    val rTmp = things.filter(t => t.attributes.keys.contains("stateMap"))
    val setupRunnerThings = things.filter(t => t.name == "setupRunnerAsThing")

    val exAbilities = ops.flatMap(o=> APIAbilityHandler.operationToAbility(o))
    val exResorces = rTmp.map(t => VD.thingToResource(t))
    val exDrivers = things.diff(rTmp).diff(setupRunnerThings).map(t=> VD.thingToDriver(t))

    //Direct launch of the VD and abilities below
    val vdID = ID.newID
    val abID = ID.newID

    publish(APIVirtualDevice.topicRequest,
      SPMessage.makeJson(
        SPHeader(from = "VolvoSimModel"),
        APIVirtualDevice.SetUpVD(
          name = "VolvoSimVD",
          id = vdID,
          exResorces, //= resources.map(_.resource),
          exDrivers, // = resources.map(_.driver),
          attributes = SPAttributes()
        )))

    publish(APIAbilityHandler.topicRequest,
      SPMessage.makeJson(
        SPHeader(from = "VolvoSimModel"),
        APIAbilityHandler.SetUpAbilityHandler(
          name = "VolvoSimAbilities",
          id = abID,
          exAbilities,
          vd = vdID
        )))
  }

  def launchOpRunner(ids : List[IDAble])= {

//    // Extract setup data from IDAbles
//    val setupRunnerThings = ids.find{t =>
//      println(s"t: ${t.name}, isit: ${t.name == "setupRunnerAsThing" && t.isInstanceOf[Thing]}")
//      t.name == "setupRunnerAsThing" && t.isInstanceOf[Thing]}.map(_.asInstanceOf[Thing])
//
//    println(setupRunnerThings)
//
//
//    setupRunnerThings.map{s =>
//      println("HOHO")
//      val exSetupRunner = APIOperationRunner.CreateRunner(thingToSetup(s))
//
//      publish(APIOperationRunner.topicRequest, SPMessage.makeJson(
//        SPHeader(from = "UnificationAbilities", to = APIOperationRunner.service), exSetupRunner))
//
//    }

  }


  def saveModel() = {

    //val modelID = ID.makeID("0d80d1d6-48cd-48ec-bfb1-d69714ef35be").get // hardcoded model id so we do not get a new model every time

    import VolvoSimModel._

    val cm = sp.models.APIModelMaker.CreateModel("VolvoSimulationVD", SPAttributes("isa" -> "VD"))

    val ops = APIAbilityHandler.abilityToOperation(command)
    val rIDable = VD.resourceToThing(resource)
    val dIDable = VD.driverToThing(driver)
    val stateVars = theThings.map(StructWrapper)
    //val setup = setupToThing(opRunner)

    val theVD = Struct(
      "TheVD",
      makeStructNodes(
        ops,
        rIDable.children(
          stateVars:_*
        ),
        dIDable
      ) ,
      SPAttributes("isa" -> "VD")
    )
    val xs = List(rIDable, dIDable, ops) ++ theThings

    val addItems = APIModel.PutItems(theVD :: List(ops) ++ xs, SPAttributes("info" -> "initial items"))

    context.system.scheduler.scheduleOnce(0.1 seconds) {
      publish(
        APIModelMaker.topicRequest,
        SPMessage.makeJson(SPHeader(from = "VolvoSimulation", to = APIModelMaker.service), cm)
      )
    }

    context.system.scheduler.scheduleOnce(0.2 seconds) {
      publish(
        APIModel.topicRequest,
        SPMessage.makeJson(SPHeader(from = "VolvoSimulation", to = cm.id.toString), addItems)
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

  def receive = {
    case s : String =>
      for { // unpack message
        mess <- SPMessage.fromJson(s)
        h <- mess.getHeaderAs[SPHeader] if  h.to == APIVDTracker.service
        b <- mess.getBodyAs[APIVDTracker.Request]
      } yield {
        var spHeader = h.swapToAndFrom
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK())) // acknowledge message received
        b match { // Check if the body is any of the following classes, and execute program
          case APIVDTracker.createModel(modelID) => saveModel()
          case APIVDTracker.launchVDAbilities(idables) => launchVDAbilities(idables)
          case APIVDTracker.launchOpRunner(idables) => launchOpRunner(idables)
        }
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))
      }
  }
  def sendAnswer(mess: String) = publish(APIVDTracker.topicResponse, mess)

}
