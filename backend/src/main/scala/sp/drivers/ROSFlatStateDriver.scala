package sp.drivers

import akka.actor._
import sp.devicehandler._
import sp.domain.Logic._
import sp.domain._
import sp.devicehandler.{APIDeviceDriver => api}
import scala.concurrent.duration._

import org.ros.message.MessageListener
import org.ros.namespace.GraphName
import org.ros.node.ConnectedNode
import org.ros.node.Node
import org.ros.node.NodeMain
import org.ros.node.NodeConfiguration
import org.ros.node.NodeMainExecutor
import org.ros.node.DefaultNodeMainExecutor
import org.ros.concurrent.CancellableLoop
import org.ros.node.topic.Publisher
import org.ros.node.topic.Subscriber
import org.ros.message.MessageFactory
import java.net.URI;
import scala.collection.JavaConverters._

/**
  * A driver for talking to ROS by mapping SP "flat state"
  * into ros messages
  *
  */
object ROSFlatStateDriver {
  val driverType = "ROSFlatStateDriver"
  def props = DriverBase.props(driverType, ROSFlatStateDriverInstance.props)
}

object ROSFlatStateDriverInstance {
  def props(d: VD.Driver) = Props(classOf[ROSFlatStateDriverInstance], d)
}

class ROSFlatStateDriverInstance(d: VD.Driver) extends Actor with NodeMain
    with ActorLogging
    with sp.service.MessageBussSupport {

  import context.dispatcher

  subscribe(api.topicRequest)

  val resourceName = d.name
  val header = SPHeader(from = d.name)
  val driverIdentifiers = d.setup.getAs[List[String]]("identifiers").getOrElse(List())

  case class RosVar(did: String, msgType: String, topic: String, field: String, rate: Int)
  val rosVars = driverIdentifiers.map(s => {
    val strs = s.split(":")
    RosVar(s, strs(0), strs(1), strs(2), if(strs.size == 4) strs(3).toInt else 0)
  })

  val didToVar = rosVars.map(r => r.did -> r).toMap
  val topics: Map[String, Iterable[RosVar]] = rosVars.groupBy(_.topic)


  var rosState: Map[String, org.ros.internal.message.Message] = Map()
  var spState: Map[String, SPValue] = Map()

  case class PublishTicker(p: Publisher[org.ros.internal.message.Message], c: Option[Cancellable])
  case class TickTopic(topic: String)
  var pubsub: Map[String, (PublishTicker,
    Subscriber[org.ros.internal.message.Message])] = Map()

  val rosNodeMainExecutor = DefaultNodeMainExecutor.newDefault()
  val masterHost = d.setup.getAs[String]("masterHost").getOrElse("localhost")
  val masterURI = d.setup.getAs[String]("masterURI").getOrElse("http://localhost:11311/")
  val nc = NodeConfiguration.newPublic(masterHost, new URI(masterURI))
  rosNodeMainExecutor.execute(this, nc)

  println("******************** ROSNODE ********************")
  println("trying to connect to master")

  override def getDefaultNodeName(): GraphName =
    GraphName.of("sp/" + d.name)

  override def onStart(cn: ConnectedNode): Unit = {
    println("******************** ROSNODE ********************")
    println("successfully connected to master " + masterHost + " @ " + masterURI)

    // to create empty ros messages
    val topicMessageFactory: MessageFactory = cn.getTopicMessageFactory()
      // create blank start state
    rosState = topics.map { case (topic, rosVars) =>
      // puke if not all topics have the same msg type
      val r = rosVars.head // we know we have head from groupBy
      assert(rosVars.forall(_.msgType == r.msgType))
      assert(rosVars.forall(_.rate == r.rate))
      topic -> topicMessageFactory.newFromType(r.msgType) }

    spState = rosVars.map(rvar => {
      val spval = getField(rvar.field, rosState(rvar.topic)).get // puke if we cant parse
      rvar.did -> spval
    }).toMap
    // start with publishing an empty state. perhaps we need support for initial state??
    publish(api.topicResponse, SPMessage.makeJson(header, APIDeviceDriver.DriverStateChange(d.name, d.id, spState)))

    // setup a listener to each topic
    pubsub = topics.map { case (topic, rosVars) =>
      val r= rosVars.head // we know we have head from groupBy
      val p: Publisher[org.ros.internal.message.Message] = cn.newPublisher(topic, r.msgType)
      // create ticker if needed
      val pt = PublishTicker(p,
        if(r.rate != 0) Some(context.system.scheduler.schedule(0 milliseconds, r.rate milliseconds, self, TickTopic(topic)))
        else None)
      val s: Subscriber[org.ros.internal.message.Message] = cn.newSubscriber(topic, r.msgType)
      s.addMessageListener(new MessageListener[org.ros.internal.message.Message] {
        override def onNewMessage(msg: org.ros.internal.message.Message) =  {
          // update internal ros state
          this.synchronized { // perhaps we can block less than this.
            rosState = rosState + (topic -> msg)

            // update sp state
            val updSpState = spState ++ rosVars.map(rvar => rvar.did -> getField(rvar.field, rosState(rvar.topic)).get).toMap

            if(updSpState != spState) {
              // new state!
              spState = updSpState
              println("============================================================")
              println("got new ros state: ")
              println(spState.mkString("\n"))
              println("============================================================")
              publish(api.topicResponse, SPMessage.makeJson(header, APIDeviceDriver.DriverStateChange(d.name, d.id, spState)))
            }
          }
        }
      })
      (topic -> (pt,s))
    }.toMap

  }

  override def onError(n: Node, t: Throwable): Unit = {
    println("******************** ROSNODE ********************")
    println("ROS ERROR NODE: " + n)
    println("ROS EXCEPTION: " + t.toString)
  }

  override def onShutdown(n: Node): Unit = {
    println("** Stopping ros node **")
  }
  override def onShutdownComplete(n: Node): Unit = println("** ros node stopped **")

  def writeStateChange(newState: Map[String, SPValue]) = {
    val toChange = newState//(newState.toSet.diff(spState.toSet)).filter(x=>didToVar.contains(x._1)).toMap
    val rosVarsToChange = toChange.map { case (did,spval) =>
      (didToVar(did) -> spval) }.toMap

    val topicsToWrite = rosVarsToChange.groupBy { case (rv,spval) => rv.topic }

    topicsToWrite.foreach { case (topic,state) =>
      val msg = rosState(topic)
      // mutate stored msg...
      state.foreach { case (rv,spval) => writeField(rv.field, spval, msg) }
      // send updated state
      println("ROS Node writing state: " + state.map(s=>s._1.field + " -> " + s._2.toString).mkString(",") + " to " + topic)
      val publisher = pubsub(topic)._1.p
      publisher.publish(msg)
    }
  }

  def rosValToSPVal(rostype: String, rv: Object): Option[SPValue] = {
    // println("type: " + rostype + " " + rv)
    rostype match {
      case "string" => Some(SPValue(rv.asInstanceOf[String]))
      case "float32" => Some(SPValue(rv.asInstanceOf[Float]))
      case "float64" => Some(SPValue(rv.asInstanceOf[Double]))
      case "bool" => Some(SPValue(rv.asInstanceOf[Boolean]))
      case "byte" => Some(SPValue(rv.asInstanceOf[Byte]))
      case "char" => Some(SPValue(rv.asInstanceOf[Byte]))
      case "int8" => Some(SPValue(rv.asInstanceOf[Byte]))
      case "uint8" => Some(SPValue(rv.asInstanceOf[Byte]))
      case "int16" => Some(SPValue(rv.asInstanceOf[Short]))
      case "uint16" => Some(SPValue(rv.asInstanceOf[Short]))
      case "int32" => Some(SPValue(rv.asInstanceOf[Int]))
      case "uint32" => Some(SPValue(rv.asInstanceOf[Int]))
      case "int64" => Some(SPValue(rv.asInstanceOf[Long]))
      case "uint64" => Some(SPValue(rv.asInstanceOf[Long]))
      case "time" => Some(SPValue(rv.toString))
      case _ =>
        println("******* ROS DRIVER TODO: ADD CONVERSION TO/FROM: " + rostype + ". Example object: " + rv.toString)
        None
    }
  }

  def spValtoRosVal(rostype: String, sv: SPValue): Option[Any] = {
    // println("type: " + rostype + " " + sv)
    rostype match {
      case "string" => Some(sv.as[String])
      case "float32" => Some(sv.as[Float])
      case "float64" => Some(sv.as[Double])
      case "bool" => Some(sv.as[Boolean])
      case "byte" => Some(sv.as[Byte])
      case "char" => Some(sv.as[Byte])
      case "int8" => Some(sv.as[Byte])
      case "uint8" => Some(sv.as[Byte])
      case "int16" => Some(sv.as[Short])
      case "uint16" => Some(sv.as[Short])
      case "int32" => Some(sv.as[Int])
      case "uint32" => Some(sv.as[Int])
      case "int64" => Some(sv.as[Long])
      case "uint64" => Some(sv.as[Long])
      // case "time" => Some(sv.as[String])
      case _ =>
        println("******* ROS DRIVER TODO: ADD CONVERSION TO/FROM: " + rostype + ". Example object: " + sv.toString)
        None
    }
  }

  def getField(fieldName: String, message: org.ros.internal.message.Message): Option[SPValue] = {
    def dig(digFor: List[String], toSearch: List[org.ros.internal.message.field.Field]): Option[SPValue] = {
      (digFor, toSearch) match {
        case (_, Nil) => None
        case (Nil, _) => None
        case (x::Nil,y::ys) => if(x == y.getName()) {
          rosValToSPVal(y.getType().getName(), y.getValue())
        } else dig(x::Nil, ys)
        case (x::xs,y::ys) =>
          if(x == y.getName()) {
            val v: Object = y.getValue()
            if(v.isInstanceOf[org.ros.internal.message.Message])
              dig(xs, v.asInstanceOf[org.ros.internal.message.Message].toRawMessage().getFields().asScala.toList)
            else {
              println(" Digging into a non-message field!")
              None
            }
          }
          else {
            dig(x::xs, ys)
          }
      }
    }
    val rm = message.toRawMessage()
    val digs = fieldName.split("\\.").toList
    val rxf = rm.getFields().asScala.toList
    dig(digs, rxf)
  }

  def writeField(fieldName: String, spval: SPValue, message: org.ros.internal.message.Message): Boolean = {
    def dig(digFor: List[String], toSearch: List[org.ros.internal.message.field.Field]): Boolean = {
      (digFor, toSearch) match {
        case (_, Nil) => false
        case (Nil, _) => false
        case (x::Nil,y::ys) => if(x == y.getName()) {
          spValtoRosVal(y.getType().getName(), spval) match {
            case Some(any) =>
              y.setValue(any.asInstanceOf[AnyRef])
              true
            case None => false
          }
        } else dig(x::Nil, ys)
        case (x::xs,y::ys) =>
          if(x == y.getName()) {
            val v: Object = y.getValue()
            if(v.isInstanceOf[org.ros.internal.message.Message])
              dig(xs, v.asInstanceOf[org.ros.internal.message.Message].toRawMessage().getFields().asScala.toList)
            else {
              println(" Digging into a non-message field!")
              None.get
              false
            }
          }
          else {
            dig(x::xs, ys)
          }
      }
    }
    val rm = message.toRawMessage()
    val digs = fieldName.split("\\.").toList
    val rxf = rm.getFields().asScala.toList
    dig(digs, rxf)
  }

  def receive = {
    case TickTopic(topic) =>
      // push current state out on ros topic
      // todo: refactor, this is a copy of the "normal" state writer
      val rosVarsToChange = spState.map { case (did,spval) =>
        (didToVar(did) -> spval) }.toMap
      val topicsToWrite = rosVarsToChange.groupBy { case (rv,spval) => rv.topic }.filter { case (t,s) => t == topic }

      topicsToWrite.foreach { case (topic,state) =>
        val msg = rosState(topic)
        // mutate stored msg...
        state.foreach { case (rv,spval) => writeField(rv.field, spval, msg) }
        // send updated state
        // println("ROS Node writing state (ticker): " + state.map(s=>s._1.field + " -> " + s._2.toString).mkString(",") + " to " + topic)
        val publisher = pubsub(topic)._1.p
        publisher.publish(msg)
      }


    case x: String =>
      SPMessage.fromJson(x).foreach{mess =>
        for {
          h <- mess.getHeaderAs[SPHeader] //if h.to == d.id.toString
          b <- mess.getBodyAs[api.Request]
        } yield {
          b match {
            case api.GetDriver =>
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom.copy(from = d.name), APISP.SPACK()))
              val body = api.TheDriver(d, spState)
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom.copy(from = d.name), body))
              publish(api.topicResponse, SPMessage.makeJson(header, APIDeviceDriver.DriverStateChange(d.name, d.id, spState)))
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom.copy(from = d.name), APISP.SPDone()))

            case api.DriverCommand(driverid, state) if driverid == d.id  =>
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom.copy(from = d.name), APISP.SPACK()))
              writeStateChange(state)
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom.copy(from = d.name), APISP.SPDone()))
              // TODO: think about only sending done when change has been registered

            // Terminating the driver
            case api.TerminateDriver(driverid) if driverid == d.id =>
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom.copy(from = d.name), APISP.SPACK()))
              rosNodeMainExecutor.shutdown()
              self ! PoisonPill
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom.copy(from = d.name), api.DriverTerminated(d.id)))
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom.copy(from = d.name), APISP.SPDone()))


            case _ =>
          }
        }
      }
  }
}
