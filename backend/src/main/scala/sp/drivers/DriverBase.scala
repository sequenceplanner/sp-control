package sp.drivers

import akka.actor._
import sp.devicehandler._
import sp.domain._
import sp.domain.Logic._

object DriverBase {

  def props(driverType: String, theDriverActor: VD.Driver => Props) =
    Props(classOf[DriverBase], driverType, theDriverActor)



  import sp.domain.SchemaLogic._

  case class DeviceDriverRequest(request: APIDeviceDriver.Request)
  case class DeviceDriverResponse(response: APIDeviceDriver.Response)

  val req: com.sksamuel.avro4s.SchemaFor[DeviceDriverRequest] = com.sksamuel.avro4s.SchemaFor[DeviceDriverRequest]
  val resp: com.sksamuel.avro4s.SchemaFor[DeviceDriverResponse] = com.sksamuel.avro4s.SchemaFor[DeviceDriverResponse]

  val apischema = makeMeASchema(
    req(),
    resp()
  )




}

/**
  * An actor used for launching the driver, which you should use to launch the driver if multiple drivers can exist
  * @param driverType
  * @param theDriverActor
  */
class DriverBase(driverType: String, theDriverActor: VD.Driver => Props) extends Actor
  with ActorLogging
  with sp.service.ServiceCommunicationSupport
  with sp.service.MessageBussSupport {

  private val id = ID.newID

  triggerServiceRequestComm(APISP.StatusResponse(
    service = driverType + "-maker",
    instanceID = Some(id),
    instanceName = s"$driverType-$id",
    tags = List("driver", "vd", "runtime", "communication"),
    api = DriverBase.apischema,
    version = 1,
    attributes = SPAttributes.empty
  ))

  subscribe(APIDeviceDriver.topicRequest)

  def receive = {
    case x: String =>
      SPMessage.fromJson(x).foreach{mess =>
        for {
          h <- mess.getHeaderAs[SPHeader]
          b <- mess.getBodyAs[APIDeviceDriver.Request]
        } yield {
          b match {
            case APIDeviceDriver.SetUpDeviceDriver(d) if d.driverType == driverType =>
              context.actorOf(theDriverActor(d), d.id.toString)
              publish(APIDeviceDriver.topicResponse, SPMessage.makeJson(h.swapToAndFrom(), APISP.SPDone()))
            case _ =>
          }
        }
      }
  }

}

