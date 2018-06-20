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
import scala.util.Try

/**
  * A driver for talking to ROS by mapping SP "flat state"
  * into ros messages
  *
  */

object ROSHelpers {
  def rosValToSPVal(rostype: String, rv: Object): Option[SPValue] = {
    try {
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
    } catch {
      case _: Throwable =>
        println("******* ROS DRIVER: PROBLEM WITH CONVERSION TO/FROM: " + rostype + ". Example object: " + rv.toString)
        None
    }
  }

  def spValtoRosVal(rostype: String, sv: SPValue): Option[Any] = {
    try {
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
    } catch {
      case _: Throwable =>
        println("******* ROS DRIVER: PROBLEM WITH CONVERSION TO/FROM: " + rostype + ". Example object: " + sv.toString)
        None
    }
  }

  var msgs: Map[String, Option[org.ros.internal.message.Message]] = Map()

  def createROSMsg(t: String): Option[org.ros.internal.message.Message] = {
    import org.ros.internal.message.definition.MessageDefinitionReflectionProvider
    import org.ros.internal.message.DefaultMessageFactory
    msgs.get(t).getOrElse {
      val m = Try {
        val mf = new DefaultMessageFactory(new MessageDefinitionReflectionProvider())
        val m: org.ros.internal.message.Message = mf.newFromType(t)
        m
      }.toOption
      msgs += (t -> m)
      m
    }
  }

  def ROSMsgToSPAttributes(msg: org.ros.internal.message.Message): Option[SPAttributes] = {
    def addFields(fields: List[org.ros.internal.message.field.Field], attr: SPAttributes): Option[SPAttributes] = {
      fields match {
        case Nil => Some(attr)
        case (f :: fs) =>
          val n = f.getName()
          val v: Object = f.getValue()
          if(v.isInstanceOf[org.ros.internal.message.Message]){
            val fields = msg.toRawMessage().getFields().asScala.toList
            // add recursively
            val spAttr = addFields(fields, SPAttributes())
            spAttr.flatMap(a=>addFields(fs, attr + (n -> a)))
          } else {
            // add field value pair
            val spval = rosValToSPVal(f.getType().getName(), f.getValue())
            spval.flatMap(v => addFields(fs, attr + (n -> v)))
          }
      }
    }
    val fields = msg.toRawMessage().getFields().asScala.toList
    addFields(fields, SPAttributes())
  }

  // def getField(fieldName: String, message: org.ros.internal.message.Message): Option[SPValue] = {
  //   def dig(digFor: List[String], toSearch: List[org.ros.internal.message.field.Field]): Option[SPValue] = {
  //     (digFor, toSearch) match {
  //       case (_, Nil) => None
  //       case (Nil, _) => None
  //       case (x::Nil,y::ys) => if(x == y.getName()) {
  //         rosValToSPVal(y.getType().getName(), y.getValue())
  //       } else dig(x::Nil, ys)
  //       case (x::xs,y::ys) =>
  //         if(x == y.getName()) {
  //           val v: Object = y.getValue()
  //           if(v.isInstanceOf[org.ros.internal.message.Message])
  //             dig(xs, v.asInstanceOf[org.ros.internal.message.Message].toRawMessage().getFields().asScala.toList)
  //           else {
  //             println(" Digging into a non-message field!")
  //             None
  //           }
  //         }
  //         else {
  //           dig(x::xs, ys)
  //         }
  //     }
  //   }

}

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

  val listenerTimeout = 5000l // after this many ms without new msgs we re-subscribe to the topic

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
  case class TickTimeout(cn: ConnectedNode, timePassed: Long) // for now theres a global timeout
  var pubsub: Map[String, (PublishTicker,
    Subscriber[org.ros.internal.message.Message])] = Map()

  var topicTimeouts: Map[String, (Long, Long)] = Map()
  var n: Option[ConnectedNode] = None

  val rosNodeMainExecutor = DefaultNodeMainExecutor.newDefault()

  // first check if we provided hosts, and uri as a setting,
  // if not, check environment variables, if they don't extist
  // default to localhost
  val localHostname = d.setup.getAs[String]("localHostname").getOrElse(scala.util.Properties.envOrElse("ROS_HOSTNAME", "localhost"))
  val masterURI = d.setup.getAs[String]("masterURI").getOrElse(scala.util.Properties.envOrElse("ROS_MASTER_URI", "http://localhost:11311/"))
  val nc = NodeConfiguration.newPublic(localHostname, new URI(masterURI))
  rosNodeMainExecutor.execute(this, nc)

  println("******************** ROSNODE ********************")
  println("trying to connect to master: " + masterURI)
  println("client settings: ")
  println("-- ROS_HOST: " + localHostname)
  println("-- ROS_IP: " + nc.getTcpRosBindAddress())

  override def getDefaultNodeName(): GraphName =
    GraphName.of("sp/" + d.name.replace('.', '_'))

  override def onStart(cn: ConnectedNode): Unit = {
    n = Some(cn)
    println("******************** ROSNODE ********************")
    println("successfully connected to master " + masterURI)

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
      println(rvar)
      val spval = getField(rvar.field, rosState(rvar.topic)).get // puke if we cant parse
      rvar.did -> spval
    }).toMap
    // start with publishing an empty state. perhaps we need support for initial state??
    publish(api.topicResponse, SPMessage.makeJson(header, APIDeviceDriver.DriverStateChange(d.name, d.id, spState)))

    topicTimeouts = topics.map { case (topic, rosVars) => (topic, (0l,listenerTimeout)) }.toMap

    // setup a listener to each topic
    pubsub = topics.flatMap { case (topic, rosVars) => createPublisherSubscriber(cn, topic, rosVars).toOption }.toMap

    // start listener timeout ticker
    context.system.scheduler.schedule(0 milliseconds, 100 milliseconds, self, TickTimeout(cn, 100l))
  }

  override def onError(n: Node, t: Throwable): Unit = {
    println("******************** ROSNODE ********************")
    println("ROS ERROR NODE: " + n)
    println("ROS EXCEPTION: " + t.toString)
  }

  override def onShutdown(n: Node): Unit = {
    println("** Stopping ros node **")
    pubsub.foreach { case (t,ps) => ps._2.removeAllMessageListeners() }
    pubsub.foreach { case (t,ps) => ps._1.p.shutdown() }
    pubsub.foreach { case (t,ps) => ps._1.c.foreach { c => c.cancel() } }
    println("** Killed pubs/subs **")
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

  def createPublisherSubscriber(cn: ConnectedNode, topic: String, rosVars: Iterable[RosVar]):
  Try[(String, (PublishTicker, Subscriber[org.ros.internal.message.Message]))] = Try {
    assert(rosVars.nonEmpty) // we know we have head from groupBy
    val r= rosVars.head
    val p: Publisher[org.ros.internal.message.Message] = cn.newPublisher(topic, r.msgType)
    // create ticker if needed
    val pt = PublishTicker(p,
      if(r.rate != 0) Some(context.system.scheduler.schedule(0 milliseconds, r.rate milliseconds, self, TickTopic(topic)))
      else None)
    val s: Subscriber[org.ros.internal.message.Message] = cn.newSubscriber(topic, r.msgType)
    s.addMessageListener(new MessageListener[org.ros.internal.message.Message] {
      override def onNewMessage(msg: org.ros.internal.message.Message) = try {
        // update internal ros state
        this.synchronized { // perhaps we can block less than this.
          topicTimeouts = topicTimeouts + (topic -> (0l,listenerTimeout))
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
            publish(api.topicResponse, SPMessage.makeJson(header, APIDeviceDriver.DriverStateChange(d.name, d.id, updSpState)))
          }
        }
      } catch { case t: Throwable => println("ROS ERROR IN LISTENER: " + t.getMessage) }
    })
    (topic -> (pt,s))
  }


  def getField(fieldName: String, message: org.ros.internal.message.Message): Option[SPValue] = {
    def dig(digFor: List[String], toSearch: List[org.ros.internal.message.field.Field]): Option[SPValue] = {
      (digFor, toSearch) match {
        case (_, Nil) => None
        case (Nil, _) => None
        case (x::Nil,y::ys) => if(x == y.getName()) {
          ROSHelpers.rosValToSPVal(y.getType().getName(), y.getValue())
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
          ROSHelpers.spValtoRosVal(y.getType().getName(), spval) match {
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
    case TickTimeout(cn, timePassed) =>
      this.synchronized {
        topicTimeouts = topicTimeouts.map { case (topic, (t, max)) =>
          topic -> (t+timePassed, max)
        }
      }
      val failed = topicTimeouts.filter { case (topic, (t, max)) => t > max }
      if (failed.nonEmpty) {
        println("****************************************")
        println("FAILED TIMEOUT for: " + failed.map(_._1).mkString(","))
        println("****************************************")

        // recreate subscribers
        this.synchronized {
          val updPubSub = topics.filter(t=>failed.exists(f=>t._1==f._1)).flatMap { case (topic, rosVars) =>
            // kill old listeners/publishers
            pubsub(topic)._2.removeAllMessageListeners()
            pubsub(topic)._1.p.shutdown()
            pubsub(topic)._1.c.foreach { c => c.cancel() }
            createPublisherSubscriber(cn, topic, rosVars).toOption
          }.toMap

          pubsub = pubsub ++ updPubSub

          // reset timers
          topicTimeouts = topicTimeouts ++ failed.map { case (topic, (t, max)) => (topic -> (0l, listenerTimeout)) }
        }
      }

    case TickTopic(topic) =>
      // push current state out on ros topic
      // todo: refactor, this is a copy of the "normal" state writer
      val rosVarsToChange = spState.map { case (did,spval) =>
        (didToVar(did) -> spval) }.toMap
      val topicsToWrite = rosVarsToChange.groupBy { case (rv,spval) => rv.topic }.filter { case (t,s) => t == topic }

      for {
        (topic, state) <- topicsToWrite
        msg <- rosState.get(topic)
      } yield {
        val msg = rosState(topic)
        // mutate stored msg...
        state.foreach { case (rv,spval) => writeField(rv.field, spval, msg) }
        // send updated state
        // println("ROS Node writing state (ticker): " + state.map(s=>s._1.field + " -> " + s._2.toString).mkString(",") + " to " + topic)
        pubsub.get(topic).foreach{ps=>ps._1.p.publish(msg) }
      }

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
              writeStateChange(state)
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom(d.name), APISP.SPDone()))
              // TODO: think about only sending done when change has been registered

            // Terminating the driver
            case api.TerminateDriver(driverid) if driverid == d.id =>
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom(d.name), APISP.SPACK()))
              rosNodeMainExecutor.shutdown()
              self ! PoisonPill
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom(d.name), api.DriverTerminated(d.id)))
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom(d.name), APISP.SPDone()))


            case _ =>
          }
        }
      }
  }
}
