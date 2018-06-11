package sp.drivers

import akka.actor.{Actor, ActorLogging, Props}
import sp.devicehandler.VD.{Driver, DriverState}
import sp.devicehandler.{APIDeviceDriver => api}
import sp.domain._

class DriverService extends Actor  with ActorLogging with  sp.service.ServiceSupport{

  val instanceID = ID.newID

  val statusResponse = DriverServiceInfo.attributes.copy( instanceID = Some(this.instanceID) )
  triggerServiceRequestComm(statusResponse)
  subscribe(api.topicRequest)
  subscribe(api.topicResponse)


  var drivers = Map[ID , (Driver, DriverState)]()
  var driversTmp = Map[ID , (Driver, DriverState)]()
  var firstTick = true

  def receive = {
    case x: String =>
      for {
        mess <- SPMessage.fromJson(x)
        h  <- mess.getHeaderAs[SPHeader]
        b <- if(mess.getBodyAs[api.Request].nonEmpty) mess.getBodyAs[api.Request] else mess.getBodyAs[api.Response]
      } yield {
        val spHeader = h.swapToAndFrom()
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))

        b match {
              case api.GetDrivers=>
                publish(api.topicRequest, SPMessage.makeJson(spHeader, api.GetDriver))
              case api.TheDriver(driver, driverState) =>
                drivers += driver.id -> (driver, driverState)
              case api.DriverTerminated(id) =>
                drivers -= id
              case other =>
        }
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))
      }

    case Tick =>
      val spHeader = SPHeader(from = DriverServiceInfo.attributes.service, reqID = instanceID)
      if(!firstTick) {
        if (drivers.keySet != driversTmp.keySet) {
          driversTmp = drivers
          sendAnswer(SPMessage.makeJson(spHeader, api.TheDrivers(drivers.values.toList)))
        }
      }
      else
        firstTick = false

      publish(api.topicRequest, SPMessage.makeJson(spHeader, api.GetDriver))

    case other =>
  }
  def sendAnswer(mess: String) = publish(api.topicResponse, mess)

  // A "ticker" that sends a "tick" string to self every 4 second
  import scala.concurrent.duration._
  import context.dispatcher
  val ticker = context.system.scheduler.schedule(4 seconds, 4 seconds, self, Tick)

}
case object Tick


object DriverService {
  def props = Props(classOf[DriverService])
}


object DriverServiceInfo {
  import sp.domain.SchemaLogic._
  case class DriverServiceRequest(request: api.Request)
  case class DriverServiceResponse(response: api.Response)

  lazy val req: com.sksamuel.avro4s.SchemaFor[DriverServiceRequest] = com.sksamuel.avro4s.SchemaFor[DriverServiceRequest]
  lazy val resp: com.sksamuel.avro4s.SchemaFor[DriverServiceResponse] = com.sksamuel.avro4s.SchemaFor[DriverServiceResponse]

  val apischema = makeMeASchema(
    req(),
    resp()
  )

  val attributes: APISP.StatusResponse = APISP.StatusResponse(
    service = "DriverService",
    tags = List("DriverService"),
    api = apischema,
    version = 1,
    topicRequest = api.topicRequest,
    topicResponse = api.topicResponse,
    attributes = SPAttributes.empty
  )
}

