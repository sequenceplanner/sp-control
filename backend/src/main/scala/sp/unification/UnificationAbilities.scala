package sp.unification

import akka.actor._
import sp.domain._
import sp.domain.Logic._
import java.util.UUID

import akka.cluster.pubsub.DistributedPubSub
import akka.cluster.pubsub.DistributedPubSubMediator.{Publish, Put, Subscribe}

import scala.util.{Failure, Success, Try}
import sp.domain.logic.{ActionParser, PropositionParser}
import sp.abilityhandler.APIAbilityHandler
import sp.devicehandler._
import sp.drivers.URDriver
import sp.models.{APIModel, APIModelMaker}
import sp.service.MessageBussSupport

import scala.concurrent.duration._

object UnificationAbilities {
  def props = Props(classOf[UnificationAbilities])
}

class UnificationAbilities extends Actor with MessageBussSupport{
  import context.dispatcher

  subscribe(APIModel.topicResponse)
  subscribe(APIModelMaker.topicResponse)

  // The resource state
  // i'm using hardcoded IDs to simplify things while testing. We need a clear model message before removing this
  val refPos = Thing(name = "refPos")
  val active = Thing(name = "active")
  val hasTool = Thing("hasTool")
  // can not change (currently we do not distinguish)
  val currentPos = Thing("currentPos")



  // abilities
  val activate = APIAbilityHandler.Ability(
    name = "activate",
    preCondition = makeCondition("!active", "active := true"),
    postCondition = makeCondition("active")
  )

  val moveTo10 = APIAbilityHandler.Ability(
    name = "moveTo10",
    preCondition = makeCondition("active && refPos == currentPos && currentPos != 10", "refPos := 10"),
    started = makeCondition("refPos = 10"),
    postCondition = makeCondition("currentPos = 10")
  )


  val moveTo20 = APIAbilityHandler.Ability(
    name = "moveTo20",
    preCondition = makeCondition("active && refPos == currentPos && currentPos != 20", "refPos := 20"),
    started = makeCondition("refPos = 20"),
    postCondition = makeCondition("currentPos = 20")
  )

  val abs = List(activate, moveTo10, moveTo20)




  def makeCondition(guard: String, actions: String*) = {

    val things: List[Thing] = List(refPos, active, hasTool, currentPos)


    val g = PropositionParser(things).parseStr(guard) match {
      case Right(p) => Some(p)
      case Left(err) => println(s"Parsing failed on condition: $guard: $err"); None
    }

    val xs = actions.flatMap { action =>
      ActionParser(things).parseStr(action) match {
        case Right(a) => Some(a)
        case Left(err) => println(s"Parsing failed on action: $action: $err"); None
      }
    }
    Condition(g.get, xs.toList)
  }



  // resource and driver setup
  // This should be moved out from this and a model should be created soon.
  val driver = VD.Driver("URDriver1", ID.newID, URDriver.driverType, SPAttributes())

  val driverResourceMapper = List(
    VD.OneToOneMapper(refPos.id, driver.id, "refPos"),
    VD.OneToOneMapper(active.id, driver.id, "active"),
    VD.OneToOneMapper(hasTool.id, driver.id, "hasTool"),
    VD.OneToOneMapper(currentPos.id, driver.id, "currentPos")
  )
  val things = List(refPos, active, hasTool, currentPos)
  val ids: Set[ID] = things.map(_.id).toSet

  val resource = VD.Resource("DummyUR", ID.newID, ids, driverResourceMapper, SPAttributes())


  // Setting up the model
  //val modelID = ID.makeID("0d80d1d6-48cd-48ec-bfb1-d69714ef35be").get // hardcoded model id so we do not get a new model every time
  // creating a model here

  val ops = abs.map(APIAbilityHandler.abilityToOperation)
  val rIDable = VD.resourceToThing(resource)
  val dIDAble = VD.driverToThing(driver)
  val xs = things ++ List(rIDable, dIDAble)
  val theVD = Struct(
    "TheVD",
    makeStructNodes(
      rIDable.children(
        refPos, active, hasTool, currentPos
      ),
      dIDAble.children()
    ) ++ makeStructNodes(ops.map(StructWrapper):_*),
    SPAttributes("isa"->"VD")
  )
  // Probably add it to a struct as well

  val cm = sp.models.APIModelMaker.CreateModel("unificationVD", SPAttributes("isa"->"VD"))
  val addItems = APIModel.PutItems(theVD :: ops ++ xs , SPAttributes("info"->"initial items"))



//  context.system.scheduler.scheduleOnce(1 seconds){
//    println("GOO")
//    publish(APIModelMaker.topicRequest, SPMessage.makeJson(SPHeader(from = "UnificationAbilities", to = APIModelMaker.service), cm))
//  }
//  context.system.scheduler.scheduleOnce(1.1 seconds){
//    println("Goo 2")
//    publish(APIModel.topicRequest,SPMessage.makeJson(SPHeader(from = "UnificationAbilities", to = cm.id.toString), addItems))}


  // Direct launch of the VD and abilities below
  val vdID = ID.newID
  val abID = ID.newID

  publish(APIVirtualDevice.topicRequest,
    SPMessage.makeJson(
      SPHeader(from = "UnificationAbilities"),
      APIVirtualDevice.SetUpVD(
        name = "UnificationVD",
        id = vdID,
        resources = List(resource),
        drivers = List(driver),
        attributes = SPAttributes()
      )))

  publish(APIAbilityHandler.topicRequest,
    SPMessage.makeJson(
      SPHeader(from = "UnificationAbilities"),
      APIAbilityHandler.SetUpAbilityHandler(
        name = "UnificationAbilites",
        id = abID,
        abilities = abs,
        vd = vdID
      )))




  // Operations
  import sp.runners._
  val actOp = Operation("activate")
  val to20 = Operation("to20", List(makeSeqCond(actOp.id, "f")))
  val to10 = Operation("to10", List(makeSeqCond(to20.id, "f")))

  val setupRunner = APIOperationRunner.CreateRunner(APIOperationRunner.Setup(
    name = "test",
    runnerID = ID.newID,
    Set(actOp, to10, to20),
    opAbilityMap = Map(
      actOp.id -> activate.id,
      to20.id -> moveTo20.id,
      to10.id -> moveTo10.id
    ),
    initialState = Map()
  ))

  publish(APIOperationRunner.topicRequest, SPMessage.makeJson(
    SPHeader(from = "UnificationAbilities", to=APIOperationRunner.service), setupRunner))




  def makeSeqCond(op: ID, state: String) = {
    Condition(guard = EQ(op, ValueHolder(state)), attributes = SPAttributes("kind"->"pre", "group"->"sequence"))
  }

  // Not doing anything, creates the model on startup
  def receive = {
    case x => //println("Ability creation for unification got: "+x)
  }

}
