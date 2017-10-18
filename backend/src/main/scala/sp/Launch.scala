package sp

import akka.actor._
import sp.example._
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
    system.actorOf(ExampleService.props, APIExampleService.service)

    val vdid = java.util.UUID.randomUUID()
    system.actorOf(sp.devicehandler.VirtualDevice.props("vd", vdid), "vd")
    val ahid = java.util.UUID.randomUUID()
    system.actorOf(sp.abilityhandler.AbilityHandler.props("ah", ahid, vdid), "ah")

    // match test
    val omid = java.util.UUID.randomUUID()
    val om = system.actorOf(sp.operationmatcher.OperationMatcher.props("operationmatcher", omid), "om")

    val dh = system.actorOf(sp.opcua.DriverHandler.props, "OPCUA")
    cluster.registerOnMemberRemoved{
      println("spcontrol node has been removed from the cluster")
      dh ! "stop"
    }

    // trucks test
    system.actorOf(sp.labkit.Trucks.props(ahid))
    // labkit test
    system.actorOf(sp.labkit.OPMakerLabKit.props, "opMakerLabKit")
    system.actorOf(sp.labkit.ProductAggregator.props, "ProductAggregator")
    system.actorOf(sp.labkit.ResourceAggregator.props, "ResourceAggregator")

    // patrik model dsl
    system.actorOf(sp.patrikmodel.PatrikModelService.props, "PatrikModel")
  }

  scala.io.StdIn.readLine("Press ENTER to exit cluster.\n")
  cluster.leave(cluster.selfAddress)

  scala.io.StdIn.readLine("Press ENTER to exit application.\n")
  system.terminate()

  Await.ready(system.whenTerminated, Duration(30, SECONDS))
  try {
    // cleanup milo crap
    import sp.milowrapper.MiloOPCUAClient
    MiloOPCUAClient.destroy()
  } catch {
    case e: Exception =>
      println("OPCUA crash - " + e.getMessage())
  }
}
