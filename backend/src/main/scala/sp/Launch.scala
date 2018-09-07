package sp

import akka.actor._
import scala.concurrent.Await
import scala.concurrent.duration._


import org.ros2.rcljava.RCLJava
import org.ros2.rcljava.executors.SingleThreadedExecutor
import org.ros2.rcljava.executors.MultiThreadedExecutor
import org.ros2.rcljava.node.BaseComposableNode

import org.ros2.rcljava.consumers.Consumer;
import org.ros2.rcljava.subscription.Subscription;

import org.ros2.rcljava.interfaces.MessageDefinition

import sp.domain._
import sp.domain.Logic._


object ROSHelpers {
  def msgToAttr(m: MessageDefinition) = {
    import scala.reflect.runtime.universe._
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
        case x => println("TODO: add support for " + x + " " + x.getClass.toString); SPValue("could not parse")
      }
      attr + (n, spval)
    })
    attr
  }

  def attrToMsg(attr: SPAttributes, m: MessageDefinition) = {
    import scala.reflect.runtime.universe._
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

class SubscriberNode extends BaseComposableNode("subscriberNode") {
  class cb[T <: MessageDefinition] extends Consumer[T] {
    def accept(msg: T) {
      val spattr = ROSHelpers.msgToAttr(msg)
      println("GOT: " + spattr)
    }
  }
  val msg = Class.forName("std_msgs.msg.String").newInstance().asInstanceOf[MessageDefinition]
  val subscription = node.createSubscription(msg.getClass, "topic", new cb)
}

import java.util.concurrent.TimeUnit
import org.ros2.rcljava.publisher.Publisher
import org.ros2.rcljava.timer.WallTimer

class PublisherNode extends BaseComposableNode("publisherNode") {
  import scala.reflect.ClassTag
  import scala.reflect._

  var count = 0

  class cb[T <: MessageDefinition](publisher: Publisher[T], msg: T) extends org.ros2.rcljava.concurrent.Callback() {
    def call() = {
      count+=1
      val str = "hej: " + count
      val attr = SPAttributes("data" -> str)
      ROSHelpers.attrToMsg(attr, msg)
      println("sending: " + attr)
      publisher.publish(msg)
    }
  }

  def pub[T <: MessageDefinition: ClassTag](msg: T) {
    val publisher: Publisher[T] = node.createPublisher[T](msg.getClass.asInstanceOf[Class[T]], "topic")
    val cb = new cb[T](publisher, msg)
    val timer = node.createWallTimer(500, TimeUnit.MILLISECONDS, cb);
  }

  val msga = Class.forName("std_msgs.msg.String").newInstance()
  pub(msga.asInstanceOf[MessageDefinition])
}


class ROS extends Actor {
  RCLJava.rclJavaInit()
  val exec = new SingleThreadedExecutor()
  val subscriberNode = new SubscriberNode()
  val publisherNode = new PublisherNode()
  exec.addNode(subscriberNode)

  // TODO: be smarter...
  import context.dispatcher
  import scala.concurrent.duration._
  val ticker = context.system.scheduler.schedule(100 millis, 100 millis, self, "spin")

  println("STARTING TIMERS")
  context.system.scheduler.scheduleOnce(5 seconds, self, "add")
  context.system.scheduler.scheduleOnce(10 seconds, self, "remove")
  context.system.scheduler.scheduleOnce(15 seconds, self, "add")
  context.system.scheduler.scheduleOnce(20 seconds, self, "remove")
  context.system.scheduler.scheduleOnce(25 seconds, self, "add")
  context.system.scheduler.scheduleOnce(30 seconds, self, "remove")
  println(" TIMERS UP")

  def receive = {
    case "spin" =>
      if(RCLJava.ok()) exec.spinOnce(100)
    case "add" =>
      println("ADDING PUBLISHER")
      exec.addNode(publisherNode)
    case "remove" =>
      println("REMOVIING PUBLISHER")
      exec.removeNode(publisherNode)
    case x => println(x)
  }

  override def postStop() = {
    RCLJava.shutdown()
  }
}


object Launch extends App {
  implicit val system = ActorSystem("SP")
  val cluster = akka.cluster.Cluster(system)

  val models = Map(
    "URModel" -> sp.unification.UnificationModel(),
    "TurtleModel" -> sp.unification.TurtleModel(),
    "DummyExample" -> sp.unification.DummyExample(),
    "ExtendedDummy" -> sp.unification.DummyExampleExtended()
  )




  cluster.registerOnMemberUp {
    // Start all you actors here.
    // println("spcontrol node has joined the cluster")
    // sp.SPCore.launch(system)

    // system.actorOf(sp.abilityhandler.AbilityHandler.props, "abilityHandlerMaker")
    // system.actorOf(sp.devicehandler.VirtualDeviceMaker.props)
    // system.actorOf(sp.drivers.URDriver.props, "URDriver")
    // system.actorOf(sp.runners.OperationRunner.props, "oprunner")
    // system.actorOf(sp.modelSupport.ModelService.props(models))
    // system.actorOf(dashboardpresets.DashboardPresetsActor())
    // system.actorOf(sp.modelImport.SPModelImport.props)
    // system.actorOf(sp.drivers.DriverService.props)

    system.actorOf(Props[ROS], name = "ros")
  }

  scala.io.StdIn.readLine("Press ENTER to exit cluster.\n")
  cluster.leave(cluster.selfAddress)

  scala.io.StdIn.readLine("Press ENTER to exit application.\n")
  system.terminate()

  Await.ready(system.whenTerminated, Duration(30, SECONDS))
  System.exit(0)
}
