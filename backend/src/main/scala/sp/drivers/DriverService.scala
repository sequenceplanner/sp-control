/*
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

  var drivers = Set[(Driver, DriverState)]()
  def receive = {

    case x: String =>
      for {
        mess <- SPMessage.fromJson(x)
        h <- mess.getHeaderAs[SPHeader] if h.to == instanceID.toString || h.to == "DriverService"
        b <- mess.getBodyAs[api.Request]
      } yield {
        var spHeader = h.swapToAndFrom
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))

        b match {
          case api.GetDrivers =>
            publish(api.topicRequest, SPMessage.makeJson(spHeader, api.GetDriver))
          case api.TheDriver(x, driverState) =>
            drivers ++=  Set((x, driverState))
            // för den sista...
            sendAnswer(SPMessage.makeJson(spHeader, api.TheDrivers(drivers)))
          case x =>
        }
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))
      }
  }
  def sendAnswer(mess: String) = publish(api.topicResponse, mess)
}

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
*/
