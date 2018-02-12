package sp

import akka.actor._
import sp.example._
import sp.modelImport._
import sp.virtcom._
import scala.concurrent.Await
import scala.concurrent.duration._

object Launch extends App {
  implicit val system = ActorSystem("SP")
  val cluster = akka.cluster.Cluster(system)

  println("Start**************************")
  println("Getting resources")
  println(this.getClass.getResource("/bundle.js"))
  println("End**************************")

  cluster.registerOnMemberUp {
    // Start all you actors here.
    println("spcontrol node has joined the cluster")
    sp.SPCore.launch(system)

    val vdid = java.util.UUID.randomUUID()
    //system.actorOf(sp.devicehandler.VirtualDevice.props("vd", vdid), "vd")
    val ahid = java.util.UUID.randomUUID()
    //system.actorOf(sp.abilityhandler.AbilityHandler.props("ah", ahid, vdid), "ah")

    // match test
//    val omid = java.util.UUID.randomUUID()
//    val om = system.actorOf(sp.operationmatcher.OperationMatcher.props("operationmatcher", omid), "om")

    val dh = system.actorOf(sp.drivers.URDriver.props, "URDriver")
    system.actorOf(sp.unification.UnificationAbilities.props(ahid), "Unification")

  }

  scala.io.StdIn.readLine("Press ENTER to exit cluster.\n")
  cluster.leave(cluster.selfAddress)

  scala.io.StdIn.readLine("Press ENTER to exit application.\n")
  system.terminate()

  Await.ready(system.whenTerminated, Duration(30, SECONDS))
}
