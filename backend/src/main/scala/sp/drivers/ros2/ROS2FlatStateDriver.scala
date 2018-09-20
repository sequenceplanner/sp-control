package sp.drivers.ros2

import akka.actor._
import sp.devicehandler._
import sp.domain.Logic._
import sp.domain._
import sp.devicehandler.{APIDeviceDriver => api}
import scala.concurrent.duration._
import sp.drivers._
import scala.util.Try

import org.ros2.rcljava.RCLJava
import org.ros2.rcljava.executors.SingleThreadedExecutor
import org.ros2.rcljava.executors.MultiThreadedExecutor
import org.ros2.rcljava.node.BaseComposableNode
import org.ros2.rcljava.consumers.Consumer;
import org.ros2.rcljava.subscription.Subscription;
import org.ros2.rcljava.publisher.Publisher
import org.ros2.rcljava.timer.WallTimer
import org.ros2.rcljava.interfaces.MessageDefinition

import akka.stream._
import akka.stream.scaladsl._
import akka.{ NotUsed, Done }

import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

/**
  * A driver for talking to ROS by mapping SP "flat state"
  * into ros messages
  *
  */

object ROSHelpers {
  def msgToAttr(m: MessageDefinition) = {
    val rm = scala.reflect.runtime.currentMirror
    val fields = rm.classSymbol(m.getClass).toType.members.collect {
      case m: TermSymbol if m.isPrivate && !m.isStatic && !m.isFinal => m
    }.toList
    val instanceMirror = rm.reflect(m)
    val attr = fields.foldLeft(SPAttributes())({case (attr,field) =>
      val n = field.name.toString
      val v = instanceMirror.reflectField(field).get
      val spval = v match {
        case v:String => SPValue(v)
        case v:Float => SPValue(v)
        case v:Double => SPValue(v)
        case v:Int => SPValue(v)
        // TODO: missing types, arrays, nesting
        case x => println("TODO: add support for " + x + " " + x.getClass.toString); SPValue("could not parse " + v.toString)
      }
      attr + (n, spval)
    })
    attr
  }

  def attrToMsg(attr: SPAttributes, m: MessageDefinition) = {
    val rm = scala.reflect.runtime.currentMirror
    val fields = rm.classSymbol(m.getClass).toType.members.collect {
      case m: TermSymbol if m.isPrivate && !m.isStatic && !m.isFinal => m
    }.toList
    val instanceMirror = rm.reflect(m)
    fields.foreach { field =>
      val n = field.name.toString
      val curval = instanceMirror.reflectField(field).get
      val spval = curval match {
        case v:String => attr.getAs[String](n).foreach(v=>instanceMirror.reflectField(field).set(v))
        case v:Float => attr.getAs[Float](n).foreach(v=>instanceMirror.reflectField(field).set(v))
        case v:Double => attr.getAs[Double](n).foreach(v=>instanceMirror.reflectField(field).set(v))
        case v:Int => attr.getAs[Int](n).foreach(v=>instanceMirror.reflectField(field).set(v))
        // TODO: missing types, arrays, nesting
        case x => println("TODO: add support for " + x + " " + x.getClass.toString)
      }
    }
  }
}

class RCLBase(system: ActorSystem) {
  implicit val executionContext = system.dispatcher
  implicit val materializer = ActorMaterializer()(system)

  RCLJava.rclJavaInit()
  val exec = new SingleThreadedExecutor()

  class SubscriberNode(msgClass: String, topic: String) extends BaseComposableNode("SPSubscriber") {
    val bufferSize = 100
    val overflowStrategy = akka.stream.OverflowStrategy.dropHead
    val queue = Source.queue[SPAttributes](bufferSize, overflowStrategy)
    val source = queue.mapMaterializedValue { case queue =>
      class cb[T <: MessageDefinition] extends Consumer[T] {
        def accept(msg: T) {
          val spattr = ROSHelpers.msgToAttr(msg)
          queue.offer(spattr)
        }
      }
      val msg = Class.forName(msgClass).newInstance().asInstanceOf[MessageDefinition]
      val subscription = node.createSubscription(msg.getClass, topic, new cb)
    }
  }

  def subscriber(msgClass: String, topic: String) = {
    val subscriberNode = new SubscriberNode(msgClass, topic)
    exec.addNode(subscriberNode)
    subscriberNode.source
  }

  class PublisherNode(msgClass: String, topic: String) extends BaseComposableNode("SPPublisher") {
    def pub[T <: MessageDefinition: ClassTag](msg: T, topic: String): Sink[SPAttributes, _] = {
      val publisher: Publisher[T] = node.createPublisher[T](msg.getClass.asInstanceOf[Class[T]], topic)
      val sink = Sink.foreach[SPAttributes](attr => {
        ROSHelpers.attrToMsg(attr, msg)
        publisher.publish(msg)
      })
      sink
    }

    val msga = Class.forName(msgClass).newInstance()
    val sink = pub(msga.asInstanceOf[MessageDefinition], topic)
  }

  def publisher(msgClass: String, topic: String) = {
    val publisherNode = new PublisherNode(msgClass, topic)
    exec.addNode(publisherNode)
    publisherNode.sink
  }

  Source.tick(initialDelay = 0.nanos, interval = 100.millis, tick = ()).runWith(Sink.foreach(_ => exec.spinOnce(90))).onComplete { _ =>
    RCLJava.shutdown()
    println("DONE")
  }
}


object ROS2FlatStateDriver {
  val driverType = "ROS2FlatStateDriver"
  def props = DriverBase.props(driverType, ROS2FlatStateDriverInstance.props)
}

object ROS2FlatStateDriverInstance {
  def props(d: VD.Driver) = Props(classOf[ROS2FlatStateDriverInstance], d)
}

class ROS2FlatStateDriverInstance(d: VD.Driver) extends Actor
    with ActorLogging
    with sp.service.MessageBussSupport {

  import context.dispatcher

  subscribe(api.topicRequest)

  implicit val materializer = ActorMaterializer()(context.system)
  val ros = new RCLBase(context.system)

  val resourceName = d.name
  val header = SPHeader(from = d.name)
  val driverIdentifiers = d.setup.getAs[List[String]]("driverIdentifiers").getOrElse(List())

  val spState: Map[String, SPValue] = Map()


  val publishers = driverIdentifiers.filter(_.startsWith("pub:")).map(_.stripPrefix("pub:"))
  val subscribers = driverIdentifiers.filter(_.startsWith("sub:")).map(_.stripPrefix("sub:"))

  case class RosVar(did: String, msgType: String, topic: String, field: String, rate: Int)
  def parseRosVar(s: String) = {
    s.split(":").toList match {
      case msg :: topic :: field :: freq :: Nil  =>
        val f = Try[Int]{freq.toInt}.getOrElse(0)
        Some(RosVar(s, msg, topic, field, f))
      case msg :: topic :: field :: Nil =>
        Some(RosVar(s, msg, topic, field, 0))
      case _ =>
        println("****************************")
        println("The driver identifier is not correctly formed for ROS!")
        println("Should be msg_type:topic:field:freq, where freq is an int or can be omitted")
        println("You wrote: " + s)
        None
    }
  }

  val pubVars = publishers.flatMap(parseRosVar)
  val subVars = subscribers.flatMap(parseRosVar)

  val pubTopics = pubVars.groupBy(_.topic)
  val subTopics = subVars.groupBy(_.topic)

  val sinks = pubTopics.map{ case (topic, rv::rest) =>
    val p = ros.publisher(rv.msgType, topic)
    // TODO: prepend rate ticking to this flow
    Flow[(String, SPAttributes)].filter(p => p._1 == topic).map(_._2).to(p)
  }

  // TODO: hacky, use grph dsl...
  val allSinks = sinks match {
    case Nil => Sink.ignore
    case first :: Nil => first
    case first :: second :: Nil => Sink.combine(first, second)(Broadcast[(String,SPAttributes)](_))
    case first :: second :: rest =>
      Sink.combine(first, second, rest:_*)(Broadcast[(String,SPAttributes)](_))
  }

  val sources = subTopics.map{ case (topic, rv::rest) => ros.subscriber(rv.msgType, topic).map(q=>(topic,q)) }
  val allSources = sources match {
    case Nil => Source.empty
    case first :: Nil => first
    case first :: second :: Nil => Source.combine(first, second)(Merge[(String,SPAttributes)](_))
    case first :: second :: rest =>
      Source.combine(first, second, rest:_*)(Merge[(String,SPAttributes)](_))
  }

  def messageToState(topic: String, message: SPAttributes): Map[String, SPValue] = {
    val l = pubTopics.get(topic).getOrElse(List())
    l.flatMap { case RosVar(id, msgType, topic, field, rate) =>
      message.value.get(field).map(spval => id -> spval)
    }.toMap
  }
  def fromMessages = Flow[(String, SPAttributes)].map(x=>messageToState(x._1,x._2)).scan(Map[String, SPValue]()){case (state, map) => state ++ map}


  def setMessageField(fieldName: String) = (v: SPValue) => SPAttributes(fieldName -> v)

  val outputMap = pubVars.map(rv => (rv.did -> rv)).toMap
  // val outputMap = Map(t1.id -> (strTopic, setMessageField("data")),
  //  x.id -> (pointTopic, setMessageField("x")), y.id -> (pointTopic, setMessageField("y")), z.id -> (pointTopic, setMessageField("z")))

  def stateToMessages(state: Map[String, SPValue]) = {
    // TODO: this does not write "half messages"
    val toWrite = pubTopics.filter { case (topic, rvs) => rvs.forall(rv=>state.keys.exists(_ == rv.did)) }
    toWrite.map { case (topic, rvs) =>
      (topic, rvs.foldLeft(SPAttributes()){case (attr, rv) =>
        attr ++ SPAttributes(rv.field -> state(rv.did))
      }) }
  }

  val toMessages = Flow[Map[String, SPValue]].mapConcat(stateToMessages)

  val inputState = allSources.via(fromMessages)
  val outputState = toMessages.alsoTo(Sink.foreach(println)).to(allSinks)


  def receive = {
    case x: String =>
      SPMessage.fromJson(x).foreach{mess =>
        for {
          h <- mess.getHeaderAs[SPHeader] //if h.to == d.id.toString
          b <- mess.getBodyAs[api.Request]
        } yield {
          b match {
            case api.GetDriver =>
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom(d.name), APISP.SPACK()))
              val body = api.TheDriver(d, spState)
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom(d.name), body))
              publish(api.topicResponse, SPMessage.makeJson(header, APIDeviceDriver.DriverStateChange(d.name, d.id, spState)))
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom(d.name), APISP.SPDone()))

            case api.DriverCommand(driverid, state) if driverid == d.id  =>
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom(d.name), APISP.SPACK()))
              /// writeStateChange(state)
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom(d.name), APISP.SPDone()))
              // TODO: think about only sending done when change has been registered

            // Terminating the driver
            case api.TerminateDriver(driverid) if driverid == d.id =>
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom(d.name), APISP.SPACK()))

              self ! PoisonPill
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom(d.name), api.DriverTerminated(d.id)))
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom(d.name), APISP.SPDone()))


            case _ =>
          }
        }
      }
  }
}
