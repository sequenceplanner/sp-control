package sp.drivers

import akka.actor._
import akka.kafka.Subscriptions
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import org.apache.kafka.clients.producer.ProducerRecord
import sp.APIHumanGUI
import sp.bluetooth.{BluetoothProxy, ProxyApplication}
import sp.devicehandler.VD.DriverState
import sp.devicehandler._
import sp.domain.Logic._
import sp.domain._
import sp.devicehandler.{APIDeviceDriver => api}
import sp.driver.APIHumanDriver
import sp.example.ExampleServiceLogic


class HumanGUIDriver extends Actor
  with ActorLogging with
  HumanServiceLogic with
  sp.service.ServiceSupport {


  def receive = {


    case x: String =>

      val bodyAPI = for {
        mess <- SPMessage.fromJson(x)
        h <- mess.getHeaderAs[SPHeader] if  h.to == APIHumanGUI.service
        b <- mess.getBodyAs[APIHumanGUI.Request]
      } yield {
        val spHeader = h.swapToAndFrom
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))

        val toSend = commands(b) // doing the logic
        sendAnswer(SPMessage.makeJson(spHeader, toSend))
//        sendAnswer(SPMessage.makeJson(spHeader, APIHumanGUI.DisplayEvent(status)))
        sendAnswer(SPMessage.makeJson(spHeader, APIHumanGUI.UserDetails(name, id)))
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))
      }

  }

  def sendAnswer(mess: String) = publish(APIHumanGUI.topicResponse, mess)
  }

/*
 * Using a trait to make the logic testable
 */
trait HumanServiceLogic {

  // This variable stores the pies that are used by the different widgets
  // Initially, it is empty
  var name = "Karen";
  var id = "A244135";

  // Matching and doing the stuff based on the message
  // This method returns multiple messages that will be sent out on the bus
  // Services should start and end with an SPACK and SPDONE if there is a
  // a clear start and end of the message stream (so listeners can unregister)
  def commands(body: APIHumanGUI.Request) = {
    body match {

      case APIHumanGUI.InputUserDetails(n, i) =>
        id = i;
        name = n;
    }
  }
}