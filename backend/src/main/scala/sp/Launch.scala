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
    system.actorOf(ExampleService.props, APIExampleService.service)
    system.actorOf(SPModelImport.props, APISPModelImport.service)
    system.actorOf(VolvoScheduler.props, APIVolvoScheduler.service)
    system.actorOf(BDDVerifier.props, APIBDDVerifier.service)

    // patrik model dsl
    system.actorOf(sp.patrikmodel.PatrikModelService.props, "PatrikModel")
  }

  scala.io.StdIn.readLine("Press ENTER to exit cluster.\n")
  cluster.leave(cluster.selfAddress)

  scala.io.StdIn.readLine("Press ENTER to exit application.\n")
  system.terminate()

  Await.ready(system.whenTerminated, Duration(30, SECONDS))
}
