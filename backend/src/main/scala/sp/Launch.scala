package sp

import akka.actor._
import scala.concurrent.Await
import scala.concurrent.duration._
import sp.domain._
import sp.domain.Logic._


object Launch extends App {
  implicit val system = ActorSystem("SP")
  val cluster = akka.cluster.Cluster(system)

  cluster.registerOnMemberUp {
    // Start all you actors here.
    println("spcontrol node has joined the cluster")
    sp.SPCore.launch(system)

    system.actorOf(sp.runners.PTMRunnerManagerOldModels.props)
    system.actorOf(sp.modelSupport.MiniModelService.props)
    system.actorOf(dashboardpresets.DashboardPresetsActor())
    system.actorOf(sp.modelImport.SPModelImport.props)
    system.actorOf(sp.drivers.ros2.RosFrontendHelper.props)
  }

  scala.io.StdIn.readLine("Press ENTER to exit cluster.\n")
  cluster.leave(cluster.selfAddress)

  scala.io.StdIn.readLine("Press ENTER to exit application.\n")
  system.terminate()

  Await.ready(system.whenTerminated, Duration(30, SECONDS))
  System.exit(0)
}
