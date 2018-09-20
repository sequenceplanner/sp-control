package sp

import akka.actor._
import scala.concurrent.Await
import scala.concurrent.duration._
import sp.domain._
import sp.domain.Logic._


object Rostest {
  def test() = {
    // val t1 = Thing("helloMessage")
    // val t2 = Thing("strInput")
    // val inputX = Thing("input.x")
    // val inputY = Thing("input.y")
    // val x = Thing("something.x")
    // val y = Thing("something.y")
    // val z = Thing("something.z")

    // val initialState = SPState("state", Map(t1.id -> SPValue("zzzzz1111"), x.id -> SPValue(2.0), y.id -> SPValue(3.0), z.id -> SPValue(4.0)))

    // val chatter = Struct("chatter", makeStructNodes(Struct("std_msgs.msg.String", makeStructNodes(t1))))
    // val points = Struct("geometry_msgs.msg.Point:/points", makeStructNodes(x,y,z))
    // val outputs = Struct(
    //   "outputs", makeStructNodes(
    //     chatter,
    //     points))

    // println(chatter)
    // println(points)
    // println(outputs)

    // implicit val materializer = ActorMaterializer()(system)
    // val ros = new ROS(system)

    // val strTopic = "/chatter"
    // val pointTopic = "/points"

    // val strPub = ros.publisher("std_msgs.msg.String", strTopic)
    // val pointPub = ros.publisher("geometry_msgs.msg.Point", pointTopic)

    // val strSubTopic = "/topic"
    // val strSub = ros.subscriber("std_msgs.msg.String", strSubTopic).map(q => (strSubTopic, q))
    // val pointSubTopic = "/points"
    // val pointSub = ros.subscriber("geometry_msgs.msg.Point", pointSubTopic).map(q => (pointSubTopic, q))
    // val allSources = Source.combine(strSub, pointSub)(x => Merge[(String, SPAttributes)](x))

    // val inputMap = Map(strSubTopic -> List((t2.id, "data")), pointSubTopic -> List((inputX.id, "x"),(inputY.id, "y")))
    // def messageToState(topic: String, message: SPAttributes): Map[ID, SPValue] = {
    //   val l = inputMap.get(topic).getOrElse(List())
    //   l.flatMap { case (id, field) =>
    //     message.value.get(field).map(spval => id -> spval)
    //   }.toMap
    // }
    // def fromMessages(initialState: SPState) = Flow[(String, SPAttributes)].map(x=>messageToState(x._1,x._2))
    //   .scan(initialState){case (state, map) => state.copy(state = state.state ++ map)}

    // val strSink = Flow[(String, SPAttributes)].filter(p => p._1 == strTopic).map(_._2).to(strPub)
    // val pointSink = Flow[(String, SPAttributes)].filter(p => p._1 == pointTopic).map(_._2).to(pointPub)
    // val allSinks = Sink.combine(strSink, pointSink)(x => Broadcast[(String,SPAttributes)](x))

    // def setMessageField(fieldName: String) = (v: SPValue) => SPAttributes(fieldName -> v)
    // val outputMap = Map(t1.id -> (strTopic, setMessageField("data")),
    //   x.id -> (pointTopic, setMessageField("x")), y.id -> (pointTopic, setMessageField("y")), z.id -> (pointTopic, setMessageField("z")))

    // def stateToMessages(state: SPState) = {
    //   state.state.filterKeys(id=>outputMap.contains(id)).groupBy { case (id, spval) => outputMap(id)._1 }.map { case (pub, l) =>
    //     (pub,
    //     l.foldLeft(SPAttributes()){ case (attr, idval) =>
    //       val f = outputMap(idval._1)._2
    //       attr ++ f(idval._2) })}.toList
    // }

    // val toMessages = Flow[SPState].mapConcat(stateToMessages)


    // val inputState = allSources.via(fromMessages(initialState))
    // val outputState = toMessages.alsoTo(Sink.foreach(println)).to(allSinks)

    // val controlLogic = Flow[SPState].map { s =>
    //   val someInput = s.state.get(t2.id)
    //   val someOutputs = someInput.map{spval =>
    //     val str = spval.as[String]
    //     val num = str.drop(14).toInt
    //     Map(t1.id -> SPValue(str.toUpperCase),
    //       x.id -> SPValue(num % 1000),
    //       y.id -> SPValue(num % 10),
    //       z.id -> SPValue(Math.sqrt((num%100).toFloat)))
    //   }.getOrElse(Map())
    //   val someInputX = s.state.get(inputX.id)
    //   val someMoreOutputs = someInputX.map(xx=>
    //     Map(x.id -> SPValue(xx.as[Int]+1))).getOrElse(Map())

    //   s.copy(state = s.state ++ someOutputs ++ someMoreOutputs)
    // }

    // val extr = Flow[SPState].expand(Iterator.continually(_))
    // val throttle = Flow[SPState].throttle(100, per = 1 second, maximumBurst = 10, mode = ThrottleMode.Shaping)

    // inputState.via(extr).via(controlLogic).via(throttle).to(outputState).run()



    // Source.single(initialState).via(toMessages).to(allSinks).run()
    // allSources.via(fromMessages).to(Sink.foreach(println)).run()



    // test transform and send to publisher
//    val subscriber = ros.subscriber("std_msgs.msg.String", "topic")
//    val publisher = ros.publisher("std_msgs.msg.String", "chatter")
//    subscriber.scan(SPAttributes("data"->"first message")){case (str, attr) => SPAttributes("data" -> attr.getAs[String]("data").getOrElse("").toUpperCase())}.to(publisher).run()

    // and print
//    subscriber.to(Sink.foreach(println)).run()

  }
}

object Launch extends App {
  implicit val system = ActorSystem("SP")
  val cluster = akka.cluster.Cluster(system)

  val models = Map(
    "UnificationDemo" -> sp.unification.UnificationModel(),
    "TurtleModel" -> sp.unification.TurtleModel(),
    "DummyExample" -> sp.unification.DummyExample(),
    "ExtendedDummy" -> sp.unification.DummyExampleExtended(),
    "SOPDummy" -> sp.unification.DummyExampleWithSOP(),
  )

  cluster.registerOnMemberUp {
    // Start all you actors here.
    println("spcontrol node has joined the cluster")
    sp.SPCore.launch(system)

    system.actorOf(sp.abilityhandler.AbilityHandler.props, "abilityHandlerMaker")
    system.actorOf(sp.devicehandler.VirtualDeviceMaker.props)
    system.actorOf(sp.runners.OperationRunner.props, "oprunner")
    system.actorOf(sp.modelSupport.ModelService.props(models))
    system.actorOf(dashboardpresets.DashboardPresetsActor())
    system.actorOf(sp.modelImport.SPModelImport.props)
    system.actorOf(sp.drivers.DriverService.props)

    // drivers
    system.actorOf(sp.drivers.URDriver.props, "URDriver")
    system.actorOf(sp.drivers.HumanDriver.props, "HumanDriver")
    system.actorOf(sp.drivers.ros2.ROS2FlatStateDriver.props, "ROSFlatStateDriver")
    system.actorOf(sp.drivers.TONDriver.props, "TONDriver")
  }

  scala.io.StdIn.readLine("Press ENTER to exit cluster.\n")
  cluster.leave(cluster.selfAddress)

  scala.io.StdIn.readLine("Press ENTER to exit application.\n")
  system.terminate()

  Await.ready(system.whenTerminated, Duration(30, SECONDS))
  System.exit(0)
}
