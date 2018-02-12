package sp.devicehandler

import java.util.UUID
import java.util.concurrent.TimeUnit

import scala.concurrent.duration._
import akka.actor._
import sp.domain._
import sp.domain.Logic._
import sp.devicehandler._





object VirtualDeviceInfo {
  import sp.domain.SchemaLogic._

  case class VirtualDeviceRequest(request: APIVirtualDevice.Request)
  case class VirtualDeviceResponse(response: APIVirtualDevice.Response)

  val req: com.sksamuel.avro4s.SchemaFor[VirtualDeviceRequest] = com.sksamuel.avro4s.SchemaFor[VirtualDeviceRequest]
  val resp: com.sksamuel.avro4s.SchemaFor[VirtualDeviceResponse] = com.sksamuel.avro4s.SchemaFor[VirtualDeviceResponse]

  val apischema = makeMeASchema(
    req(),
    resp()
  )

  private val id = ID.newID
  val attributes: APISP.StatusResponse = APISP.StatusResponse(
    service = "VirtualDevice",
    instanceID = Some(id),
    instanceName = s"VD-$id",
    tags = List("virtual device", "vd", "runtime", "communication"),
    api = apischema,
    version = 1,
    attributes = SPAttributes.empty
  )
}


// TODO: Make these persistant if needed in the future...


object VirtualDeviceMaker {
  def props = Props(classOf[VirtualDeviceMaker])
}

class VirtualDeviceMaker extends Actor
  with ActorLogging
  with sp.service.ServiceCommunicationSupport
  with sp.service.MessageBussSupport {

  import context.dispatcher

  // start waiting for ping requests from service handler
  triggerServiceRequestComm(VirtualDeviceInfo.attributes.copy(
    service = "VirtualDeviceMaker")
  )

  subscribe(APIVirtualDevice.topicRequest)

  var vds: Map[ID, ActorRef] = Map()

  override def receive = {
    case x: String =>
      val mess = SPMessage.fromJson(x)
      for {
        m <- mess
        h <- m.getHeaderAs[SPHeader] // add filter here if we want multiple makers
        b <- m.getBodyAs[APIVirtualDevice.Request] if b.isInstanceOf[APIVirtualDevice.SetUpVD]
        setup = b.asInstanceOf[APIVirtualDevice.SetUpVD]
      } yield {
        val updH = h.swapToAndFrom
        if (vds.contains(setup.id)){
          publish(APIVirtualDevice.topicResponse, SPMessage.makeJson(updH, APISP.SPError(s"VD with id ${setup.id} already exist")))
        } else {
          val a = context.actorOf(VirtualDevice.props(setup))
          vds += setup.id -> a
          context.watch(a)
          publish(APIVirtualDevice.topicResponse, SPMessage.makeJson(updH, APISP.SPDone()))
        }
      }
    case Terminated(x) => vds = vds.filter(_._2 == x)
  }


}



object VirtualDevice {
  def props(setup: APIVirtualDevice.SetUpVD) = Props(classOf[VirtualDevice], setup)
}

class VirtualDevice(setup: APIVirtualDevice.SetUpVD) extends Actor
    with ActorLogging
    with VirtualDeviceLogic
    with sp.service.ServiceCommunicationSupport
    with sp.service.MessageBussSupport {

  import context.dispatcher

  val name = setup.name
  val id = setup.id

  subscribe(APIVirtualDevice.topicRequest)
  subscribe(APIDeviceDriver.topicResponse)

  val statusResponse = VirtualDeviceInfo.attributes.copy(instanceID = Some(id))
  triggerServiceRequestComm(statusResponse)

  initVD

  override def receive = {
    case x: String =>
      val mess = SPMessage.fromJson(x)
      for {
        m <- mess
        h <- m.getHeaderAs[SPHeader]
        b <- m.getBodyAs[APIVirtualDevice.Request]
      } yield {
        b match {
          case r : APIVirtualDevice.VDCommand =>
            val ackHeader = h.copy(reply = VirtualDeviceInfo.attributes.service)
            publish(APIVirtualDevice.topicResponse, SPMessage.makeJson(ackHeader, APISP.SPACK()))

            val diffs = getDriverDiffs(r)

            val doneHeader = h.copy(reply = VirtualDeviceInfo.attributes.service)
            if(diffs.isEmpty || diffs.forall { case (k,v) => v.isEmpty }) {
              println("No variables to update... Sending done immediately for request: " + h.reqID)
              publish(APIVirtualDevice.topicResponse, SPMessage.makeJson(doneHeader, APISP.SPDone()))
            } else {
              val commands = getDriverCommands(diffs)
              val requests = commands.map { command =>
                // send commands to the drivers
                val header = SPHeader(from = id.toString)
                publish("driverCommands", SPMessage.makeJson(header, command))
                header.reqID
              }
              activeDriverRequests += (h.reqID -> requests.toList)
              // start timeout counter
              if(r.timeout > 0) {
                val dct = DriverCommandTimeout(h.reqID, r.timeout)
                context.system.scheduler.scheduleOnce(Duration(r.timeout, TimeUnit.MILLISECONDS), self, dct)
              }
            }
          case APIVirtualDevice.GetVD =>
            val updh = h.swapToAndFrom
            publish(APIVirtualDevice.topicResponse, SPMessage.makeJson(updh, makeVDMessage))
            publish(APIVirtualDevice.topicResponse, SPMessage.makeJson(updh, APISP.SPDone()))



          case x => println("todo: " + x)
        }
      }

      for {
        m <- mess
        h <- m.getHeaderAs[SPHeader]
        b <- m.getBodyAs[APIDeviceDriver.Response]
      } yield {
        b match {

          case e @ APIDeviceDriver.DriverStateChange(name, did, state, _) =>
            //println("got a statechange:" + e)
            val oldrs = resourceState
            driverEvent(e)
            //println("new driver state: " + driverState)
            //println("new resource state: " + resourceState)

            resourceState.filter { case (nid, ns) =>
              oldrs.get(nid) match {
                case Some(os) => (ns.toSet diff os.toSet).nonEmpty
                case None => true
              }
            }.foreach { case (rid, state) if resources.contains(rid) =>
              val header = SPHeader(from = id.toString)
              val body = APIVirtualDevice.StateEvent(resources(rid).r.name, rid, state)
              // hur ska vi ha det med event/answer-topics?
              // publish("events", SPMessage.makeJson(header, body))
              publish(APIVirtualDevice.topicResponse, SPMessage.makeJson(header, body))
            }

          case APIDeviceDriver.DriverCommandDone(reqid, success) =>
            val request = activeDriverRequests.filter { case (rid,reqs) => reqs.contains(reqid) }
            activeDriverRequests = request.headOption match {
              case Some((rid,reqs)) =>
                if(!success) {
                  val errorHeader = SPHeader(reqID = reqid, from = VirtualDeviceInfo.attributes.service)
                  publish(APIVirtualDevice.topicResponse, SPMessage.makeJson(errorHeader, APISP.SPError("driver command failed")))
                  activeDriverRequests - rid
                } else {
                  val nr = reqs.filter(_!=reqid)
                  if(nr.isEmpty) {
                    val doneHeader = SPHeader(reqID = reqid, from = VirtualDeviceInfo.attributes.service)
                    publish(APIVirtualDevice.topicResponse, SPMessage.makeJson(doneHeader, APISP.SPDone()))
                    activeDriverRequests - rid
                  } else {
                    activeDriverRequests + (rid -> nr)
                  }
                }
              case None => activeDriverRequests
            }



          case x => println("todo: " + x)
        }
      }

    case DriverCommandTimeout(request, timeout) =>
      activeDriverRequests.get(request).map { reqs =>
        println("Driver command(s) timed out after " + timeout + "ms")
        println(" failed driver requests: " + reqs.mkString(", "))
      }
      activeDriverRequests = activeDriverRequests - request
  }

  def initVD = {
    val h = SPHeader(from = id.toString)
    setup.drivers.foreach{d =>
      newDriver(d)
      publish(APIDeviceDriver.topicRequest, SPMessage.makeJson(h, APIDeviceDriver.SetUpDeviceDriver(d)))
    }
    setup.resources.foreach(newResource)
    publish(APIVirtualDevice.topicRequest, SPMessage.makeJson(h, makeVDMessage))

  }

  def makeVDMessage = {
    APIVirtualDevice.TheVD(
      name = setup.name,
      id = setup.id,
      resources = setup.resources.map(r => VD.ResourceWithState(r, resourceState.getOrElse(r.id, Map[ID, SPValue]()))),
      drivers = setup.drivers.map(d => VD.DriverWithState(d, driverState.getOrElse(d.id, Map[String, SPValue]()))),
      attributes = setup.attributes
    )
  }

}


trait VirtualDeviceLogic {
  val name: String
  val id: UUID

  case class DriverCommandTimeout(requestID: ID, timeout: Int)

  case class StateReader(f: (Map[ID, VD.DriverState], Map[ID, VD.State]) =>  Map[ID, VD.State])
  case class StateWriter(f: (Map[ID, VD.State], Map[ID, VD.DriverState]) =>  Map[ID, VD.DriverState])

  case class ResourceWithState(r: VD.Resource, read: List[StateReader], write: List[StateWriter])

  var drivers: Map[ID, VD.Driver] = Map()
  var driverState: Map[ID, VD.DriverState] = Map()
  var activeDriverRequests: Map[ID, List[ID]] = Map()

  var resources: Map[ID, ResourceWithState] = Map()
  var resourceState: Map[ID, VD.State] = Map()

  def newDriver(d: VD.Driver) = {
    drivers += d.id -> d
    driverState += d.id -> Map[String, SPValue]()
  }

  def newResource(resource: VD.Resource) = {
    val rw = resource.stateMap.flatMap {
      case VD.OneToOneMapper(t, id, name) =>
        val reader = StateReader{ case (drivers, resources) =>
          val nr = for {
            driver <- drivers.get(id)
            value <- driver.get(name)
            rs <- resources.get(resource.id)
          } yield {
            resources + (resource.id -> (rs + (t -> value)))
          }
          nr.getOrElse(resources)
        }
        val writer = StateWriter{ case (resources, drivers) =>
          val nd = for {
            rs <- resources.get(resource.id)
            value <- rs.get(t)
            driver <- drivers.get(id)
          } yield {
            drivers + (id -> (driver + (name -> value)))
          }
          nd.getOrElse(drivers)
        }
        Some((reader,writer))
      // potentially add other mapping types here
    }

    resources += resource.id -> ResourceWithState(resource, rw.map(_._1), rw.map(_._2))
    resourceState += resource.id -> Map()
  }

  def driverEvent(e: APIDeviceDriver.DriverStateChange) = {
    val current = driverState.get(e.id)
    current.foreach{ state =>
      val upd = state ++ e.state
      driverState += e.id -> upd
    }
    resourceState = resources.foldLeft(resourceState){ case (rs, r) =>
      r._2.read.foldLeft(rs){ case (rs, reader) => reader.f(driverState, rs)}}
  }

  def getDriverDiffs(c: APIVirtualDevice.VDCommand) = {
    val diffs = for {
      r <- resources.get(c.resource)
    } yield {
      val s = r.write.foldLeft(driverState) { case (ds,writer) =>
        writer.f(Map(c.resource -> c.stateRequest), ds)
      }
      s.map { case (k,v) =>
        val m = driverState.getOrElse(k, v)
        val d = v.toSet diff m.toSet
        (k, d.toMap)
      }
    }
    diffs.getOrElse(Map())
  }

  def getDriverCommands(diffs: Map[ID, VD.DriverState]) = {
    for {
      (did,stateDiff) <- diffs if stateDiff.nonEmpty
      d <- drivers.get(did)
    } yield {
      APIDeviceDriver.DriverCommand(d.id, stateDiff)
    }
  }

}
