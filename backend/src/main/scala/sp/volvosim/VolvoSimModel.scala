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


  // a transport
  val running = Thing("running")
  val start = Thing("start")
  val bodyID = Thing("bodyID") // body b1 (hårdkodar en body)
  val bodyPos = Thing("bodyPos") // body b1 (hårdkodar en body)
  val s1 = Thing("s1") // sensor s1
  val s2 = Thing("s2") // sensor s2
  val newBodyID = Thing("newBodyID")
  val newBodyLength = Thing("newBodyLength")


  // pressure
  val act = Thing("act")
  val ref = Thing("ref")
  val atRef = Thing("atRef")


  val robdriver = VD.Driver("volvoSimulatedRobotDriver", ID.newID, DummyVolvoRobotDriver.driverType, SPAttributes())
  val transpdriver = VD.Driver("volvoSimulatedTransportDriver", ID.newID, VolvoTransportSimulationDriver.driverType, SPAttributes())
  val pressdriver = VD.Driver("volvoSimulatedPressureDriver", ID.newID, VolvoPressureSimulationDriver.driverType, SPAttributes())

  val theThingsMap: Map[Thing, ID] = Map(
    currentProgram -> robdriver.id,
    currentTime -> robdriver.id,
    eStop -> robdriver.id,
    homePosition -> robdriver.id,
    running -> transpdriver.id,
    start -> transpdriver.id,
    bodyID -> transpdriver.id,
    bodyPos -> transpdriver.id,
    s1 -> transpdriver.id,
    s2 -> transpdriver.id,
    newBodyID -> transpdriver.id,
    newBodyLength -> transpdriver.id,
    act -> pressdriver.id,
    ref -> pressdriver.id,
    atRef -> pressdriver.id
  )
  val theThings = theThingsMap.keySet.toList
  val ids: Set[ID] = theThings.map(_.id).toSet

  val opcSetup = SPAttributes("url" -> "opc.tcp://129.16.37.101:8070", "identifiers" -> theThings.map(_.name))
  val opc = VD.Driver("opclocal", ID.newID, "OPCUA", opcSetup)
  val drivers = List(robdriver, transpdriver, pressdriver)

  val driverResourceMapper = theThingsMap.map { case (t, d) =>
    // Must have the same name as the state
    OneToOneMapper(t.id, d, t.name)
  }.toList

  val resource = VD.Resource("VolvoSimulated", ID.newID, ids, driverResourceMapper, SPAttributes())

  val listenerMap: Map[ID, String] = Map(
    newBodyID.id -> "newBodyID",
    s1.id -> "s1",
    running.id -> "running",

  )

  val listenerResource = VD.Resource("VolvoSimulatedListener", ID.newID, ids, driverResourceMapper, SPAttributes())




  // The ability hardcoded prog (this is so we can test by running abilities).
  // Change so the ability expect the prog name later
  val transportStart = APIAbilityHandler.Ability(
    name = s"transport.start",
    parameters = List(),
    preCondition = makeCondition(
      "pre",
      "!start",
      "start := true")(theThings),
    started = makeCondition("started",s"running")(theThings),
    postCondition = makeCondition("post", "running")(theThings),
    resetCondition = makeCondition("reset", "true")(theThings)
  )
  val transportStop = APIAbilityHandler.Ability(
    name = s"transport.stop",
    parameters = List(),
    preCondition = makeCondition(
      "pre",
      "start",
      "start := false")(theThings),
    started = makeCondition("started",s"!running")(theThings),
    postCondition = makeCondition("post", "!running")(theThings),
    resetCondition = makeCondition("reset", "true")(theThings)
  )

  val transportAddNewCarB1 = APIAbilityHandler.Ability(
    name = s"transport.newCar",
    parameters = List(),
    preCondition = makeCondition(
      kind = "pre",
      guard = "bodyID == empty",
      actions = s"newBodyID := b1", "newBodyLength := 10")(theThings),
    started = makeCondition("started",s"newBodyID == b1")(theThings),
    postCondition = makeCondition("post", "true")(theThings),
    resetCondition = makeCondition("reset", "true")(theThings)
  )

  val robotProg = APIAbilityHandler.Ability(
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



  val pressurize = APIAbilityHandler.Ability(
    name = s"pressurize",
    parameters = List(),
    preCondition = makeCondition(
      "pre",
      "ref != 15",
      "ref := 15")(theThings),
    started = makeCondition("started",s"act != ref")(theThings),
    postCondition = makeCondition("post", "act == 15")(theThings),
    resetCondition = makeCondition("reset", "true")(theThings)
  )

  val depressurize = APIAbilityHandler.Ability(
    name = s"depressurize",
    parameters = List(),
    preCondition = makeCondition(
      "pre",
      "ref != 0",
      "ref := 0")(theThings),
    started = makeCondition("started",s"act != ref")(theThings),
    postCondition = makeCondition("post", "act == 0")(theThings),
    resetCondition = makeCondition("reset", "true")(theThings)
  )


  val commands = List(transportStart, transportStop, transportAddNewCarB1, robotProg, pressurize, depressurize)





  // operations

  val sensor1 = Thing("sensor1")
  val sensor2 = Thing("sensor2")

  val theVars = List(sensor1, sensor2)
  val startTransport = Operation(name = "startTransport", conditions = List())
  val newBody = Operation(name = "newBody", conditions = List())
  val startRobot = Operation(name = "startRobot", conditions = List())
  val startPressure = Operation(name = "startPressure", conditions = List(
    makeCondition(
      kind = "pre",
      guard = "sensor1 == true"
    )(theVars)
  ))
  val startDepressurize = Operation(name = "startDepressurize", conditions = List())

  import sp.domain.logic.SOPLogic._

  val bodyOps = ((1 until 10).map { i => newBody.copy(name = newBody.name + i, id = ID.newID) }).toList
  val theSOPStart = Sequence((List(startTransport) ++ bodyOps).map(o=>OperationNode(o.id)))

  val robOps = ((1 until 10).map { i =>
    List(
      (startPressure.copy(name = startPressure.name + i, id = ID.newID) -> pressurize.id),
      (startRobot.copy(name = startRobot.name + i, id = ID.newID) -> robotProg.id),
      (startDepressurize.copy(name = startDepressurize.name + i, id = ID.newID) -> depressurize.id))}).toList.flatten

  val theSOPRobot = Sequence(robOps.map(o=>OperationNode(o._1.id)))


  val ops = List(startTransport) ++ bodyOps ++ robOps.map(_._1)
  val conds = extractOperationConditions(List(theSOPStart, theSOPRobot), "sequence")
  val updOps = for {
    o <- ops
    c <- conds.get(o.id)
  } yield (o.copy(conditions = c :: o.conditions ))
  val updOps2 = updOps ++ ops.filterNot(o => updOps.exists(o2=>o2.id == o.id))

  val opRunner = APIOperationRunner.Setup(
    name = "VolvoSim_Runner",
    runnerID = ID.newID,
    ops = updOps2.toSet,
    opAbilityMap = Map(startTransport.id -> transportStart.id)
      ++ bodyOps.map(bo => (bo.id -> transportAddNewCarB1.id)).toMap
      ++ robOps.map(ro => (ro._1.id -> ro._2)).toMap,
    initialState = Map(
      sensor1.id -> SPValue(false),
      sensor1.id -> SPValue(false)),
    variableMap = Map(
      sensor1.id -> s1.id,
      sensor2.id -> s2.id
    ),
    abilityParameters = Map()
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


  // hack for opcua
  subscribe(APIVirtualDevice.topicResponse)


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

    // Extract setup data from IDAbles
    val setupRunnerThings = ids.find{t =>
      println(s"t: ${t.name}, isit: ${t.name == "setupRunnerAsThing" && t.isInstanceOf[Thing]}")
      t.name == "setupRunnerAsThing" && t.isInstanceOf[Thing]}.map(_.asInstanceOf[Thing])

    println(setupRunnerThings)


    setupRunnerThings.map{s =>
      println("HOHO")
      val exSetupRunner = APIOperationRunner.CreateRunner(thingToSetup(s))

      publish(APIOperationRunner.topicRequest, SPMessage.makeJson(
        SPHeader(from = "VolvoSim", to = APIOperationRunner.service), exSetupRunner))
    }

  }


  def saveModel() = {

    //val modelID = ID.makeID("0d80d1d6-48cd-48ec-bfb1-d69714ef35be").get // hardcoded model id so we do not get a new model every time

    import VolvoSimModel._

    val cm = sp.models.APIModelMaker.CreateModel("VolvoSimulationVD", SPAttributes("isa" -> "VD"))

    val ops = commands.map(APIAbilityHandler.abilityToOperation)
    val rIDable = VD.resourceToThing(resource)
    val rListenerIDable = VD.resourceToThing(listenerResource)
    val dIDable = drivers.map(VD.driverToThing)
    val stateVars = theThings.map(StructWrapper)
    val setup = setupToThing(opRunner)

    val theVD = Struct(
      "TheVD",
      makeStructNodes(ops.map(StructWrapper):_*) ++
      makeStructNodes(dIDable.map(StructWrapper):_*) ++
      makeStructNodes(
        rIDable.children(
          stateVars:_*
        ),
        rListenerIDable.children(
          stateVars:_*
        ),
        setup
      ) ,
      SPAttributes("isa" -> "VD")
    )
    val xs = setup :: rListenerIDable :: rIDable :: dIDable ++ ops ++ theThings

    val addItems = APIModel.PutItems(theVD :: xs, SPAttributes("info" -> "initial items"))

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

  import sp.opcua._
  import sp.milowrapper._
  val opcclient = new MiloOPCUAClient()
  val ok = opcclient.connect("opc.tcp://10.0.101.37:4870")
  println("CLIENT CONNECTED OK? " + ok)


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


      for { // unpack message
        mess <- SPMessage.fromJson(s)
        h <- mess.getHeaderAs[SPHeader]
        b <- mess.getBodyAs[APIVirtualDevice.Response]
      } yield {
        var spHeader = h.swapToAndFrom
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK())) // acknowledge message received
        b match { // Check if the body is any of the following classes, and execute program

          case APIVirtualDevice.StateEvent(resource, id, state, diff) if resource == VolvoSimModel.listenerResource.name =>
            val m = VolvoSimModel.theThings
            println("****************************************")
            println(s" GOT STATE EVENT FOR RESOURCE ${resource}")
            println(s" state: " + state)
            println("map: " + m)
            val mapped = m.map { case t =>
              for {
                vv <- state.get(t.id)
              } yield (t.name -> vv) }.flatten.toMap
            println(s" mapping " + mapped)
            mapped.foreach { case (node, value) => opcclient.write(node, value) }
            println("****************************************")
          case _ =>
        }
      }
  }
  def sendAnswer(mess: String) = publish(APIVDTracker.topicResponse, mess)

}
