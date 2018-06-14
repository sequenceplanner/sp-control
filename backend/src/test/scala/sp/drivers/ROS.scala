package sp.drivers

import akka.actor._
import akka.cluster.pubsub.DistributedPubSubMediator.{Publish, Subscribe}
import akka.cluster.pubsub._
import akka.testkit._
import com.typesafe.config._
import org.scalatest._
import sp.devicehandler._
import sp.domain.Logic._
import sp.domain._

import scala.concurrent.duration._


/**
 * Testing AbilityActor
 */
class ROSTest extends FreeSpecLike with Matchers {
  import ROSHelpers._

  // test message reflection
  for {
    msg <- createROSMsg("std_msgs/String")
    attr <- ROSMsgToSPAttributes(msg)
  } yield {
    println(attr)
  }

  for {
    msg <- createROSMsg("unification_roscontrol/MiRPoseUniToSP")
    attr <- ROSMsgToSPAttributes(msg)
  } yield {
    println(attr)
  }

}
