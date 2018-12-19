package sp.drivers.ros2

import akka.actor._
import sp.domain.Logic._
import sp.domain._
import sp.streams.SPStreamSupport._
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
  def rosMsgFormatToJavaClass(msgType: String) = {
    // val source = "unification_ros2_messages/MoveItSPToUni"
    // val target = "unification_ros2_messages.msg.MoveItUniToSP"
    val s = msgType.split("/")
    for {
      p <- s.lift(0)
      m <- s.lift(1)
    } yield {
      p + ".msg." + m
    }
  }

  def createROSMsg(msgType: String): Option[MessageDefinition] = {
    rosMsgFormatToJavaClass(msgType).flatMap { msgClass => Try {
      Class.forName(msgClass).newInstance().asInstanceOf[MessageDefinition]
    }.toOption
    }
  }

  def msgToAttr(m: MessageDefinition): SPAttributes = {
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
        case v:Byte => SPValue(v)
        case v:Int => SPValue(v)
        case v:Long => SPValue(v)
        case v:Boolean => SPValue(v)
        case v:MessageDefinition => msgToAttr(v)
        case v:java.util.ArrayList[_] =>
          import scala.collection.JavaConverters._
          val l = v.asScala.toList
          SPValue(l.map {
            case v:String => SPValue(v)
            case v:Float => SPValue(v)
            case v:Double => SPValue(v)
            case v:Byte => SPValue(v)
            case v:Int => SPValue(v)
            case v:Long => SPValue(v)
            case v:Boolean => SPValue(v)
            case v: MessageDefinition => msgToAttr(v)
            case x => println("TODO: add support for " + x + " " + x.getClass.toString); SPValue("could not parse " + v.toString)
          })

        // TODO: add missing types, clean up code
        case x => println("TODO: add support for " + x + " " + x.getClass.toString); SPValue("could not parse " + v.toString)
      }
      attr + (n, spval)
    })
    attr
  }

  def attrToMsg(attr: SPAttributes, m: MessageDefinition): Unit = {
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
        case v:Byte => attr.getAs[Byte](n).foreach(v=>instanceMirror.reflectField(field).set(v))
        case v:Int => attr.getAs[Int](n).foreach(v=>instanceMirror.reflectField(field).set(v))
        case v:Long => attr.getAs[Long](n).foreach(v=>instanceMirror.reflectField(field).set(v))
        case v:Boolean => attr.getAs[Boolean](n).foreach(v=>instanceMirror.reflectField(field).set(v))
        case f:MessageDefinition => attr.getAs[SPAttributes](n).foreach{v=>
          // omg... this is scary stuff
          attrToMsg(v,f)
        }
        case l:java.util.ArrayList[_] =>
          import scala.collection.JavaConverters._

          for {
            elementType <- field.typeSignature.typeArgs.headOption
            spval <- attr.get(n)
          } yield {
            spval match {
              case play.api.libs.json.JsArray(newElements) =>




                // if(emptyMsg.isInstanceOf[MessageDefinition]) {
                //   // ros message -- element needs to be a spattribute
                //   attrToMsg(e.as[SPAttributes],emptyMsg.asInstanceOf[MessageDefinition])
                //   println(msgToAttr(emptyMsg.asInstanceOf[MessageDefinition]))
                //   emptyMsg
                // } else
                {
                  // built in type
                  if(elementType == typeOf[java.lang.String]) {
                    val nn = newElements.map(_.as[String])
                    val jn = new java.util.ArrayList[String](nn.asJava)
                    instanceMirror.reflectField(field).set(jn)
                  } else if(elementType <:< typeOf[MessageDefinition]) {
                    println("message type: " + elementType.toString)
                    val nn = newElements.flatMap { e =>
                      Try { Class.forName(elementType.toString).newInstance().asInstanceOf[MessageDefinition] }.toOption.map{emptyMsg => attrToMsg(e.as[SPAttributes],emptyMsg); emptyMsg }
                    }
                    println("NN IS: " + nn)
                    val jn = new java.util.ArrayList[MessageDefinition](nn.asJava)
                    instanceMirror.reflectField(field).set(jn)
                  }
                  // TODO... some how do this a better way
                  // TODO also add missing PODs

                }

              case _ =>
            }
          }

        // TODO: add missing types, cleanup
        case x => println("TODO: add support for " + x + " " + x.getClass.toString)
      }
    }
  }
}

class RCLBase(system: ActorSystem) {
  implicit val executionContext = system.dispatcher
  implicit val materializer = ActorMaterializer()(system)

  RCLManager.rclInit()
  val exec = new SingleThreadedExecutor()

  class SubscriberNode(msgType: String, topic: String) extends BaseComposableNode("SPSubscriber") {
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
      val msg = ROSHelpers.createROSMsg(msgType).get
      val subscription = node.createSubscription(msg.getClass, topic, new cb)
    }
  }

  def subscriber(msgClass: String, topic: String) = {
    val subscriberNode = new SubscriberNode(msgClass, topic)
    exec.addNode(subscriberNode)
    subscriberNode.source
  }

  class PublisherNode(msgType: String, topic: String) extends BaseComposableNode("SPPublisher") {
    def pub[T <: MessageDefinition: ClassTag](msg: T, topic: String): Sink[SPAttributes, _] = {
      val publisher: Publisher[T] = node.createPublisher[T](msg.getClass.asInstanceOf[Class[T]], topic)
      val sink = Sink.foreach[SPAttributes](attr => {
        ROSHelpers.attrToMsg(attr, msg)
        publisher.publish(msg)
      })
      sink
    }

    val msg = ROSHelpers.createROSMsg(msgType).get
    val sink = pub(msg, topic)
  }

  def publisher(msgClass: String, topic: String) = {
    val publisherNode = new PublisherNode(msgClass, topic)
    exec.addNode(publisherNode)
    publisherNode.sink
  }

  Source.tick(initialDelay = 0.nanos, interval = 100.millis, tick = ()).runWith(Sink.foreach(_ => exec.spinOnce(90))).onComplete { _ =>
    RCLManager.rclShutdown()
  }
}


object RCLManager {
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
}
