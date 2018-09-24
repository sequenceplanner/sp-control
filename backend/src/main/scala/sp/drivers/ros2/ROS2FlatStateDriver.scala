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
  def createROSMsg(msgClass: String): Option[MessageDefinition] = Try {
    Class.forName(msgClass).newInstance().asInstanceOf[MessageDefinition]
  }.toOption

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
        case v:Long => SPValue(v)
        case v:Boolean => SPValue(v)
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
        case v:Long => attr.getAs[Long](n).foreach(v=>instanceMirror.reflectField(field).set(v))
        case v:Boolean => attr.getAs[Boolean](n).foreach(v=>instanceMirror.reflectField(field).set(v))
        // TODO: missing types, arrays, nesting
        case x => println("TODO: add support for " + x + " " + x.getClass.toString)
      }
    }
  }
}

class RCLBase(system: ActorSystem) {
  implicit val executionContext = system.dispatcher
  implicit val materializer = ActorMaterializer()(system)

  ROS2FlatStateDriver.rclInit()
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
    ROS2FlatStateDriver.rclShutdown()
  }
}


object ROS2FlatStateDriver {
  val driverType = "ROS2FlatStateDriver"

  // rcl init/destroy
  var rclIsInit = 0

  def rclInit() = this.synchronized {
    if(rclIsInit == 0) RCLJava.rclJavaInit()
    rclIsInit += 1
  }

  def rclShutdown() = this.synchronized {
    rclIsInit -= 1
    if(rclIsInit == 0) {
      RCLJava.shutdown()
    }
  }

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
  val driverIdentifiers = d.setup.getAs[List[String]]("identifiers").getOrElse(List())

  println("ROS2 drivers: " + driverIdentifiers)

  val publishers = driverIdentifiers.filter(_.startsWith("pub:"))
  val subscribers = driverIdentifiers.filter(_.startsWith("sub:"))

  case class RosVar(did: String, msgType: String, topic: String, field: String, rate: Int)
  def parseRosVar(s: String) = {
    s.split(":").toList match {
      case pubsub :: msg :: topic :: field :: freq :: Nil  =>
        val f = Try[Int]{freq.toInt}.getOrElse(0)
        Some(RosVar(s, msg, topic, field, f))
      case pubsub :: msg :: topic :: field :: Nil =>
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

  // create blank state for publishing topics
  var spState: Map[String, SPValue] = pubVars.map{r =>
    val field = ROSHelpers.createROSMsg(r.msgType).flatMap{ msg =>
      ROSHelpers.msgToAttr(msg).get(r.field).map(spval => r.did -> spval)
    }
    field.getOrElse(r.did -> SPValue(0))
  }.toMap

  val pubTopics = pubVars.groupBy(_.topic)
  val subTopics = subVars.groupBy(_.topic)

  val sinks = pubTopics.map{ case (topic, rv::rest) =>
    val p = ros.publisher(rv.msgType, topic)
    // TODO: prepend rate ticking to this flow
    // start with an "empty" ros message, merge changes to field within the stream
    val empty = ROSHelpers.createROSMsg(rv.msgType).map(ROSHelpers.msgToAttr).getOrElse(SPAttributes())
    val partialMessages = Flow[SPAttributes].scan(empty){ case (attr, partial) => attr ++ partial }
    Flow[(String, SPAttributes)].filter(p => p._1 == topic).map(_._2).via(partialMessages).to(p)
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
    val l = subTopics.get(topic).getOrElse(List())
    l.flatMap { case r@RosVar(id, msgType, topic, field, rate) =>
      message.value.get(field).map(spval => id -> spval)
    }.toMap
  }
  val keepState = Flow[Map[String, SPValue]].scan(Map[String, SPValue]()){ case (state, map) => state ++ map}

  def fromMessages = Flow[(String, SPAttributes)].map(x=>messageToState(x._1,x._2))

  def stateToMessages(state: Map[String, SPValue]) = {
    pubTopics.map { case (topic, rvs) =>
      (topic, rvs.foldLeft(SPAttributes()){case (attr, rv) =>
        attr ++ state.get(rv.did).map(spval => SPAttributes(rv.field -> spval)).getOrElse(SPAttributes())
      }) }
  }

  val toMessages = Flow[Map[String, SPValue]].mapConcat(stateToMessages)

  val inputState = allSources.via(fromMessages).via(keepState)
  val outputState = toMessages.to(allSinks)

  // *********
  // above should be kept, below is temp to work with old virtual device
  // *********

  inputState.to(Sink.foreach { state =>
    spState = spState ++ state
    publish(api.topicResponse, SPMessage.makeJson(header, APIDeviceDriver.DriverStateChange(d.name, d.id, spState)))
  }).run()

  val outputQueue = Source.queue[Map[String, SPValue]](100, akka.stream.OverflowStrategy.dropHead).to(outputState).run()

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

              /// write driver commands to our out stream
              spState = spState ++ state
              outputQueue.offer(state)

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
