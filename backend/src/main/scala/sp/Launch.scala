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

  cluster.registerOnMemberUp {
    // Start all you actors here.
    println("spcontrol node has joined the cluster")
    sp.SPCore.launch(system)
    system.actorOf(sp.abilityhandler.AbilityHandler.props, "abilityHandlerMaker")
    system.actorOf(sp.devicehandler.VirtualDeviceMaker.props)
    val dh = system.actorOf(sp.drivers.URDriver.props, "URDriverH")
    val rosh = system.actorOf(sp.drivers.ROSDriver.props, "ROSDriverH")
    val humanH = system.actorOf(sp.drivers.HumanDriver.props, "HumanDriverH")
    system.actorOf(sp.runners.OperationRunner.props, "oprunnerH")


  }

  system.actorOf(sp.unification.UnificationROSModel.props, "UnificationAbilityMaker")
  scala.io.StdIn.readLine("Press ENTER to exit cluster.\n")
  cluster.leave(cluster.selfAddress)

  scala.io.StdIn.readLine("Press ENTER to exit application.\n")
  system.terminate()

  Await.ready(system.whenTerminated, Duration(30, SECONDS))
}
