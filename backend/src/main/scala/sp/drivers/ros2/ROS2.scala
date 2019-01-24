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

  def javaClassToRosMsgFormat(msgType: String) = {
    // val source = "unification_ros2_messages.msg.MoveItUniToSP"
    // val target = "unification_ros2_messages/MoveItSPToUni"
    val s = msgType.split("\\.msg\\.")
    for {
      p <- s.lift(0)
      m <- s.lift(1)
    } yield {
      p + "/" + m
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
    val className = m.getClass().getCanonicalName()
    val rosMsg = javaClassToRosMsgFormat(className)
    rosMsg.map(mn => attr + ("_type" -> mn)).getOrElse(attr)
  }

  def attrToMsg(attr: SPAttributes, messageType: String = ""): MessageDefinition = {
    val msgType = if(messageType != "") messageType else attr.getAs[String]("_type").getOrElse("")

    val m = createROSMsg(msgType) match {
      case Some(msg) => msg
      case _ => throw new Exception("ROS messageType not found: " + msgType + " -- " + attr)
    }

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
          val className = f.getClass().getCanonicalName()
          val rosMsg = javaClassToRosMsgFormat(className).getOrElse("")
          val newMsg = attrToMsg(v, rosMsg)
          instanceMirror.reflectField(field).set(newMsg)
        }
        case l:java.util.ArrayList[_] =>
          import scala.collection.JavaConverters._

          for {
            elementType <- field.typeSignature.typeArgs.headOption
            spval <- attr.get(n)
          } yield {
            spval match {
              case play.api.libs.json.JsArray(newElements) if elementType =:= typeOf[java.lang.String] =>
                val nn = newElements.map(_.as[String])
                val jn = new java.util.ArrayList[String](nn.asJava)
                instanceMirror.reflectField(field).set(jn)
              case play.api.libs.json.JsArray(newElements) if elementType =:= typeOf[java.lang.Double] =>
                val nn = newElements.map(_.as[Double])
                val jn = new java.util.ArrayList[Double](nn.asJava)
                instanceMirror.reflectField(field).set(jn)
              case play.api.libs.json.JsArray(newElements) if elementType =:= typeOf[java.lang.Float] =>
                val nn = newElements.map(_.as[Float])
                val jn = new java.util.ArrayList[Float](nn.asJava)
                instanceMirror.reflectField(field).set(jn)
              case play.api.libs.json.JsArray(newElements) if elementType =:= typeOf[java.lang.Byte] =>
                val nn = newElements.map(_.as[Byte])
                val jn = new java.util.ArrayList[Byte](nn.asJava)
                instanceMirror.reflectField(field).set(jn)
              case play.api.libs.json.JsArray(newElements) if elementType =:= typeOf[java.lang.Integer] =>
                val nn = newElements.map(_.as[Int])
                val jn = new java.util.ArrayList[Int](nn.asJava)
                instanceMirror.reflectField(field).set(jn)
              case play.api.libs.json.JsArray(newElements) if elementType =:= typeOf[java.lang.Long] =>
                val nn = newElements.map(_.as[Long])
                val jn = new java.util.ArrayList[Long](nn.asJava)
                instanceMirror.reflectField(field).set(jn)
              case play.api.libs.json.JsArray(newElements) if elementType =:= typeOf[java.lang.Boolean] =>
                val nn = newElements.map(_.as[Boolean])
                val jn = new java.util.ArrayList[Boolean](nn.asJava)
                instanceMirror.reflectField(field).set(jn)
              case play.api.libs.json.JsArray(newElements) if elementType <:< typeOf[MessageDefinition] =>
                val nn = newElements.map { e => attrToMsg(e.as[SPAttributes], javaClassToRosMsgFormat(elementType.toString).getOrElse("")) }
                val jn = new java.util.ArrayList[MessageDefinition](nn.asJava)
                instanceMirror.reflectField(field).set(jn)
              case play.api.libs.json.JsArray(newElements) =>
                println("TODO: add array support for " + elementType)
                throw new Exception("ros message fail")

              case x => throw new Exception("UNEXPECTED SPVAL: " + x)
            }
          }

        // TODO: add missing types, cleanup
        case x => println("TODO: add support for " + x + " " + x.getClass.toString)
      }
    }
    m
  }

}

class RCLBase(system: ActorSystem) {
  implicit val executionContext = system.dispatcher
  implicit val materializer = ActorMaterializer()(system)

  var exec: SingleThreadedExecutor = null
  if(RCLManager.rclInit()) {
    exec = new SingleThreadedExecutor()
    Source.tick(initialDelay = 0.nanos, interval = 100.millis, tick = ()).runWith(Sink.foreach(_ => exec.spinOnce(90))).onComplete { _ =>
      RCLManager.rclShutdown()
    }
  }

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
    if(exec == null) Source.repeat(SPAttributes("error!" -> "ros not initialized. check your sourcing")) else {
      val subscriberNode = new SubscriberNode(msgClass, topic)
      exec.addNode(subscriberNode)
      subscriberNode.source
    }
  }

  class PublisherNode(msgType: String, topic: String) extends BaseComposableNode("SPPublisher") {
    def pub[T <: MessageDefinition: ClassTag](msg: T, topic: String): Sink[SPAttributes, _] = {
      val publisher: Publisher[T] = node.createPublisher[T](msg.getClass.asInstanceOf[Class[T]], topic)
      val sink = Sink.foreach[SPAttributes](attr => {
        val toSend = ROSHelpers.attrToMsg(attr, msgType).asInstanceOf[T]
        publisher.publish(toSend)
      })
      sink
    }

    val msg = ROSHelpers.createROSMsg(msgType).getOrElse { throw new Exception(s"$msgType could not be created"); null }
    val sink = pub(msg, topic)
  }

  def publisher(msgClass: String, topic: String) = {
    if(exec == null) Sink.ignore else {
      val publisherNode = new PublisherNode(msgClass, topic)
      exec.addNode(publisherNode)
      publisherNode.sink
    }
  }

}


object RCLManager {
  // rcl init/destroy
  var rclIsInit = 0

  def rclInit(): Boolean = this.synchronized {


    // beatiful API design in ros java here, it will call system.exit
    // on exception... we need to handle that. so we do it with a
    // terrible hack

    class SMExit extends SecurityManager {
      override def checkPermission( permission: java.security.Permission  ) {
        if( permission.getName().startsWith("exitVM") ) {
          throw new Exception("not allow to call exit()")
        }
      }
    }

    if(rclIsInit == 0) {
      try {
        System.setSecurityManager( new SMExit() )
        RCLJava.rclJavaInit()
        System.setSecurityManager( null )
        rclIsInit += 1
        true
      } catch {
        case t:Throwable =>
          println("Cannot initialize rosjava: " + t.getMessage())
          println("Did you source your ROS workspaces?")
          System.setSecurityManager( null )
          false
      }
    } else {
      rclIsInit += 1
      true
    }
  }

  def rclShutdown() = this.synchronized {
    rclIsInit -= 1
    if(rclIsInit == 0) {
      try {
        RCLJava.shutdown()
      } catch {
        case t:Throwable =>
      }
    }
  }
}
