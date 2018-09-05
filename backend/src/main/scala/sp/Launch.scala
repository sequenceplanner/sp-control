package sp

import akka.actor._
import scala.concurrent.Await
import scala.concurrent.duration._




import org.ros2.rcljava.RCLJava
import org.ros2.rcljava.executors.SingleThreadedExecutor
import org.ros2.rcljava.executors.MultiThreadedExecutor
import org.ros2.rcljava.node.BaseComposableNode

// public class Composed {
//   public static void main(final String[] args) throws InterruptedException, Exception {
//     // Initialize RCL
//       try {
//     RCLJava.rclJavaInit();
//       } catch (Exception e) {
//           System.out.println("AJ");
//           System.out.println("MESSAGE: " + e.getMessage());
//       }
//     SingleThreadedExecutor exec = new SingleThreadedExecutor();
//     SubscriberNode subscriberNode = new SubscriberNode();
//     PublisherNode publisherNode = new PublisherNode();
//     exec.addNode(subscriberNode);
//     exec.addNode(publisherNode);
//     exec.spin();
//   }
// }

import org.ros2.rcljava.consumers.Consumer;
import org.ros2.rcljava.subscription.Subscription;

class SubscriberNode extends BaseComposableNode("subscriberNode") {
  object cb extends Consumer[std_msgs.msg.String] {
    def accept(msg: std_msgs.msg.String) {
      println("hej! got: " + msg.getData())
    }
  }
  val subscription = node.createSubscription(classOf[std_msgs.msg.String], "topic", cb)
}

// public class SubscriberNode extends BaseComposableNode {
//   private Subscription<std_msgs.msg.String> subscription;

//   public SubscriberNode() {
//     super("subscriber_node");
//     subscription = node.<std_msgs.msg.String>createSubscription(std_msgs.msg.String.class, "topic",
//         msg -> System.out.println("Subscriber [" + msg.getData() + "]"));
//   }
// }


import java.util.concurrent.TimeUnit
import org.ros2.rcljava.publisher.Publisher
import org.ros2.rcljava.timer.WallTimer

class PublisherNode extends BaseComposableNode("publisherNode") {
  val publisher = node.createPublisher(classOf[std_msgs.msg.String], "topic")

  val timer = node.createWallTimer(500, TimeUnit.MILLISECONDS, cb);
  var count = 0
  object cb extends org.ros2.rcljava.concurrent.Callback() {
    def call() = {
      count+=1
      val x = new std_msgs.msg.String()
      val str = "hej: " + count
      println("sending: " + str)
      x.setData(str)
      publisher.publish(x)
    }
  }
}


class ROS extends Actor {
  RCLJava.rclJavaInit()
  val exec = new SingleThreadedExecutor()
  val subscriberNode = new SubscriberNode()
  val publisherNode = new PublisherNode()
  exec.addNode(subscriberNode)
  exec.addNode(publisherNode)

  // TODO: be smarter...
  import context.dispatcher
  import scala.concurrent.duration._
  val ticker = context.system.scheduler.schedule(10 millis, 10 millis, self, "spin")

  def receive = {
    case "spin" =>
      if(RCLJava.ok()) exec.spinOnce()
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

  val rosActor = system.actorOf(Props[ROS], name = "ros")

  cluster.registerOnMemberUp {
    // Start all you actors here.
    println("spcontrol node has joined the cluster")
    sp.SPCore.launch(system)

    system.actorOf(sp.abilityhandler.AbilityHandler.props, "abilityHandlerMaker")
    system.actorOf(sp.devicehandler.VirtualDeviceMaker.props)
    system.actorOf(sp.drivers.URDriver.props, "URDriver")
    system.actorOf(sp.runners.OperationRunner.props, "oprunner")
    system.actorOf(sp.modelSupport.ModelService.props(models))
    system.actorOf(dashboardpresets.DashboardPresetsActor())
    system.actorOf(sp.modelImport.SPModelImport.props)
    system.actorOf(sp.drivers.DriverService.props)

  }

  scala.io.StdIn.readLine("Press ENTER to exit cluster.\n")
  cluster.leave(cluster.selfAddress)

//  rosActor ! "stop"

  scala.io.StdIn.readLine("Press ENTER to exit application.\n")
  system.terminate()

  Await.ready(system.whenTerminated, Duration(30, SECONDS))
  System.exit(0)
}
