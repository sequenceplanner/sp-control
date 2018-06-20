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
  subscribe(sp.vdtesting.APIVDTracker.topicRequest)

  val Online = "Online"
  val Offline  = "Offline"
  val Unresponsive  = "Unresponsive"



  var drivers = Map[ID , (Driver, DriverState, String)]()
  var driversTmp = Map[ID , (Driver, DriverState, String)]()
  var firstTick = true

  def receive = {
    case x: String =>
      for {
        mess <- SPMessage.fromJson(x)
        h  <- mess.getHeaderAs[SPHeader]
        b <- if(mess.getBodyAs[api.Request].nonEmpty) {mess.getBodyAs[api.Request]}
                else{ if(mess.getBodyAs[api.Response].nonEmpty) {mess.getBodyAs[api.Response]} else {mess.getBodyAs[sp.vdtesting.APIVDTracker.Request ]}}
      } yield {
        val spHeader = h.swapToAndFrom()
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))

        b match {
              case api.GetDrivers=>
                sendAnswer(SPMessage.makeJson(spHeader, api.TheDrivers(drivers.values.toList)))
              case api.TheDriver(driver, driverState) =>
                drivers += driver.id -> (driver, driverState, Online) // if a new or already existing driver is received, the map should be updated with the driver, state and active status: Online
              case api.DriverTerminated(id) => // Todo: Sometimes this message is not received, instead the driver will show as unresponsive even tho it is terminated
                drivers += id -> drivers(id).copy(_3 = Offline) // if the driver is terminated, its active status is set to Offline
              case sp.vdtesting.APIVDTracker.ResetGUI =>
                drivers = Map()
              case other =>
        }
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))
      }


    case Tick =>
      val spHeader = SPHeader(from = DriverServiceInfo.attributes.service, reqID = instanceID)
      if(!firstTick) {
        if (! theSame(drivers, driversTmp)) { // Check if the maps have the same keys and active drivers..
          driversTmp = drivers
          sendAnswer(SPMessage.makeJson(spHeader, api.TheDrivers(drivers.values.toList)))
        }
        drivers = drivers.map(d => d._1 -> (d._2.copy(_3 = if(d._2._3 == Online) Unresponsive else d._2._3))) // Set all active drivers status to Unresponsive, (the active drivers should be updated between ticks)
      }
      else
        firstTick = false

      publish(api.topicRequest, SPMessage.makeJson(spHeader, api.GetDriver))

    case other =>
  }
  def sendAnswer(mess: String) = publish(api.topicResponse, mess)

  def theSame(M1 : Map[ID , (Driver, DriverState, String)] , M2 : Map[ID , (Driver, DriverState, String)]) : Boolean = {
    if (M1.keySet != M2.keySet) // check if driver IDs are different
      return false
    else {
      if (M1.values.map(_._3).toList != M2.values.map(_._3).toList) // Check if the driver active status is different
        return false
      else
        return true
    }
  }
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

