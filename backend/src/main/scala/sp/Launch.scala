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


    system.actorOf(sp.abilityhandler.AbilityHandler.props, "abilityHandlerMaker")

    // match test
//    val omid = java.util.UUID.randomUUID()
//    val om = system.actorOf(sp.operationmatcher.OperationMatcher.props("operationmatcher", omid), "om")

    system.actorOf(sp.devicehandler.VirtualDeviceMaker.props)
    val dh = system.actorOf(sp.drivers.URDriver.props, "URDriver")
    system.actorOf(sp.unification.UnificationAbilities.props, "UnificationAbilityMaker")
    system.actorOf(sp.runners.OperationRunner.props, "oprunner")
  }

  scala.io.StdIn.readLine("Press ENTER to exit cluster.\n")
  cluster.leave(cluster.selfAddress)

  scala.io.StdIn.readLine("Press ENTER to exit application.\n")
  system.terminate()

  Await.ready(system.whenTerminated, Duration(30, SECONDS))
}
