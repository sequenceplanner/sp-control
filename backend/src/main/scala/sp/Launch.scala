package sp

import akka.actor._
import sp.example._
import sp.modelImport._
import sp.virtcom._
import scala.concurrent.Await
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
import java.net.URI;

class ROSNode() extends NodeMain {

  println("******************** ROSNODE ********************")
  println("class instantiated")


  override def getDefaultNodeName(): GraphName =
    GraphName.of("sp/rosdriver")

  override def onStart(cn: ConnectedNode): Unit = {
    println("******************** ROSNODE ********************")
    println("got on start: " + cn.toString)

    // // sp state identifier -> value
    // var state: Map[String, SPValue] = Map()

    // // ros state topic -> value
    // var rosState: Map[String,



    var publisher: Publisher[std_msgs.String] = cn.newPublisher("chatter", std_msgs.String._TYPE)
    var publisher2: Publisher[geometry_msgs.Twist] = cn.newPublisher("/turtle1/cmd_vel", geometry_msgs.Twist._TYPE)

    var listener: Subscriber[std_msgs.String] = cn.newSubscriber("/test/cmd", std_msgs.String._TYPE)




    import scala.collection.JavaConverters._
    import org.ros.message.MessageFactory
    import sp.domain.SPValue

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

    // to create empty ros messages
    val topicMessageFactory: MessageFactory = cn.getTopicMessageFactory();

    val driverIdentifiers = List("geometry_msgs/Twist:/turtle1/cmd_vel:linear.x",
      "geometry_msgs/Twist:/turtle1/cmd_vel:linear.y")

    case class RosVar(did: String, msgType: String, topic: String, field: String)
    val rosVars = driverIdentifiers.map(s => {
      val strs = s.split(":")
      RosVar(s, strs(0), strs(1), strs(2)) })

    val didToVar = rosVars.map(r => r.did -> r).toMap
    val topics: Map[String, Iterable[RosVar]] = rosVars.groupBy(_.topic)

    // create blank start state
    var rosState: Map[String, org.ros.internal.message.Message] =
      topics.map { case (topic, r::rosVars) =>
        // puke if not all topics have the same msg type
        assert((r::rosVars).forall(_.msgType == r.msgType))
        topic -> topicMessageFactory.newFromType(r.msgType) }

    var spState: Map[String, SPValue] = rosVars.map(rvar => {
      val spval = getField(rvar.field, rosState(rvar.topic)).get // puke if we cant parse
      rvar.did -> spval
    }).toMap

    // setup a listener to each topic
    val pubsub = topics.map { case (topic, rosVars@r::rs) =>
      val p: Publisher[org.ros.internal.message.Message] = cn.newPublisher(topic, r.msgType)
      val s: Subscriber[org.ros.internal.message.Message] = cn.newSubscriber(topic, r.msgType)
      s.addMessageListener(new MessageListener[org.ros.internal.message.Message] {
        override def onNewMessage(msg: org.ros.internal.message.Message) =  {
          // update internal ros state
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
          }
        }
      })
      (topic -> (p,s))
    }.toMap

    def writeStateChange(newState: Map[String, SPValue]) = {
      val toChange = (newState.toSet.diff(spState.toSet)).toMap
      val rosVarsToChange = toChange.map { case (did,spval) =>
        (didToVar(did) -> spval) }.toMap

      val topicsToWrite = rosVarsToChange.groupBy { case (rv,spval) => rv.topic }

      topicsToWrite.foreach { case (topic,state) =>
        val msg = rosState(topic)
        // mutate stored msg...
        state.foreach { case (rv,spval) => writeField(rv.field, spval, msg) }
        val publisher = pubsub(topic)._1
        // send updated state
        println("Writing " + state.map(_._2).mkString(",") + " to " + topic)
        publisher.publish(msg)
      }
    }

    Thread.sleep(5000)

    val testState = Map(
      "geometry_msgs/Twist:/turtle1/cmd_vel:linear.x" -> SPValue(10)
    )
    writeStateChange(testState)

    Thread.sleep(5000)

    val testState2 = Map(
      "geometry_msgs/Twist:/turtle1/cmd_vel:linear.x" -> SPValue(10),
      "geometry_msgs/Twist:/turtle1/cmd_vel:linear.y" -> SPValue(10)
    )
    writeStateChange(testState2)

    Thread.sleep(5000)

    val testState3 = Map(
      "geometry_msgs/Twist:/turtle1/cmd_vel:linear.x" -> SPValue(0),
      "geometry_msgs/Twist:/turtle1/cmd_vel:linear.y" -> SPValue(0)
    )
    writeStateChange(testState3)

    Thread.sleep(5000)


    // val x: org.ros.internal.message.Message = topicMessageFactory.newFromType("geometry_msgs/Twist")
    // val res = getField("linear.y", x)
    // println("1 ******************         " + res)
    // val spval = SPValue(-1.0)
    // writeField("linear.y", spval, x)
    // val res2 = getField("linear.y", x)
    // println("2 ******************         " + res2)



    // var pp: Publisher[org.ros.internal.message.Message] = cn.newPublisher("/turtle1/cmd_vel", "geometry_msgs/Twist")

    // var lastTurtleCmd: org.ros.internal.message.Message = null

    // var ll: Subscriber[org.ros.internal.message.Message] = cn.newSubscriber("/turtle1/cmd_vel", "geometry_msgs/Twist")
    // ll.addMessageListener(new MessageListener[org.ros.internal.message.Message] {
    //   override def onNewMessage(msg: org.ros.internal.message.Message) =  {
    //     lastTurtleCmd = msg

    //     val res = getField("linear.x", msg)
    //     println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>: " + res)

    //     res.foreach { spval =>
    //       val newZ = SPValue(spval.as[Double] * -1)
    //       writeField("linear.y", newZ, msg)
    //       pp.publish(msg)
    //     }
    //   }
    // })



    // var listener2: Subscriber[turtlesim.Pose] = cn.newSubscriber("/turtle1/pose", turtlesim.Pose._TYPE)
    // var exit = false


    // var l3: Subscriber[org.ros.internal.message.Message] = cn.newSubscriber("/turtle1/pose", "turtlesim/Pose")

    // listener.addMessageListener(new MessageListener[std_msgs.String] {
    //   override def onNewMessage(msg: std_msgs.String) {
    //     println("GOT MESSAGE: " + msg.getData())
    //     if (msg.getData() == "exit") exit = true
    //   }
    // });



    // l3.addMessageListener(new MessageListener[org.ros.internal.message.Message] {
    //   override def onNewMessage(msg: org.ros.internal.message.Message) =  {
    //     val rm = msg.toRawMessage()

    //     rm.getFields().asScala.find(f=>f.getName()=="x").foreach {f =>
    //       println("xxxxxxxxxx: " + f.getType())
    //     }
    //     val digx = rm.getFields().asScala.find(f=>f.getName()=="x").foreach {f =>
    //       println("xxxxxxxxxx: " + f.getValue())
    //     }

    //     println("GOT TURTLE POS: X: " + rm.getType() + " is equal to ") // + turtlesim.Pose._TYPE)
    //     val fields = rm.getFields().asScala

    //     println("GOT TURTLE POS: X: " + fields.map(f => f.getName() + " " + f.getType()).mkString(" "))

    //     // val xrm = x.toRawMessage()
    //     // val xfields = rm.getFields().asScala
    //     // println("PROVIDE TURTLE POS: X: " + xrm.getType() + " is equal to " + rm.getType())
    //     // println("PROVIDE TURTLE POS: X: " + xfields.map(f => f.getName() + " " + f.getType()).mkString(" "))

    //   }
    // })

    // listener2.addMessageListener(new MessageListener[turtlesim.Pose] {
    //   var left = false;
    //   override def onNewMessage(msg: turtlesim.Pose) {
    //     println("GOT TURTLE POS: X: " + msg.getX() + "Y: " + msg.getY())
    //     println("GOT TURTLE POS: X: " + msg.toRawMessage().getType() + " is equal to " + turtlesim.Pose._TYPE)
    //     val typ = msg.toRawMessage().getType()
    //     val fields = msg.toRawMessage().getFields().asScala

    //     println("GOT TURTLE POS: X: " + fields.map(f => f.getName() + " " + f.getType()).mkString(" "))



    //     val twist: geometry_msgs.Twist = publisher2.newMessage();
    //     if(msg.getX() > 7) {
    //       left = true;
    //     } else if(msg.getX() < 3) {
    //       left = false;
    //     }

    //     val xvel = if (left) -1 else 1

    //     twist.getLinear().setX(xvel);
    //     publisher2.publish(twist);
    //   }
    // });


    // var sequenceNumber: Int = 0

    // cn.executeCancellableLoop(new CancellableLoop() {
    //   override def setup(): Unit = {
    //     sequenceNumber = 0
    //   }

    //   override def loop(): Unit = {
    //     if (exit) {
    //       cn.shutdown()
    //     } else {
    //       val str: std_msgs.String = publisher.newMessage()
    //       str.setData("Hello world! " + sequenceNumber)
    //       println("publishing: " + str.getData())
    //       publisher.publish(str)
    //       sequenceNumber = sequenceNumber + 1
    //       Thread.sleep(1000)
    //     }
    //   }
    // });


  }

  override def onError(n: Node, t: Throwable): Unit = {
    println("******************** ROSNODE ********************")
    println("ROS ERROR NODE: " + n)
    println("ROS EXCEPTION: " + t.toString)
  }

  override def onShutdown(n: Node): Unit = println("** Stopping ros node **")
  override def onShutdownComplete(n: Node): Unit = println("** ros node stopped **")

}

object ROSNode {

  def apply(): ROSNode = new ROSNode()

}





object Launch extends App {
  implicit val system = ActorSystem("SP")
  val cluster = akka.cluster.Cluster(system)

  val rosNodeMainExecutor = DefaultNodeMainExecutor.newDefault();

  cluster.registerOnMemberUp {
    // Start all you actors here.
    println("spcontrol node has joined the cluster")
    sp.SPCore.launch(system)

    // system.actorOf(sp.abilityhandler.AbilityHandler.props, "abilityHandlerMaker")
    // system.actorOf(sp.devicehandler.VirtualDeviceMaker.props)
    // val dh = system.actorOf(sp.drivers.URDriver.props, "URDriverH")
    // val rosh = system.actorOf(sp.drivers.ROSDriver.props, "ROSDriverH")
    // val humanH = system.actorOf(sp.drivers.HumanDriver.props, "HumanDriverH")
    // system.actorOf(sp.runners.OperationRunner.props, "oprunnerH")
    // system.actorOf(sp.unification.UnificationROSModel.props, "UnificationROS")


    val node = ROSNode()
    val nc = NodeConfiguration.newPublic("localhost", new URI("http://localhost:11311/"))
    rosNodeMainExecutor.execute(node, nc)
  }

  scala.io.StdIn.readLine("Press ENTER to exit cluster.\n")
  cluster.leave(cluster.selfAddress)

  rosNodeMainExecutor.shutdown()

  scala.io.StdIn.readLine("Press ENTER to exit application.\n")
  system.terminate()

  Await.ready(system.whenTerminated, Duration(30, SECONDS))
  System.exit(0)
}
