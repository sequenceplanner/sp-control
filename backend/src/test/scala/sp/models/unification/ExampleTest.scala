package sp.models.unification.example

import org.scalatest._

import akka.testkit._
import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import akka.stream._
import akka.stream.scaladsl._
import akka.{ NotUsed, Done }
import scala.concurrent.duration._
import scala.concurrent._

import sp.domain.Logic._
import sp.domain._

import sp.streams._

import sp.runners._
import sp.runners.Shared._



class ExampleTest(_system: ActorSystem) extends TestKit(_system) with FreeSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("SP", ConfigFactory.parseString(
    """
      | akka.actor.provider = "cluster"
      | akka.remote.netty.tcp.port=0
      | akka.remote.artery.canonical.port=0
    """.stripMargin)))

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }


  println("\n\n\n")
  println(System.getProperty("java.class.path"))
  println("\n\n\n")

  println("\n\n\n")
  println(System.getProperty("java.library.path"))
  println("\n\n\n")

  import sp.drivers.ros2.ROSHelpers

  // check that the messages exist in the path
  val stateMsg = ROSHelpers.createROSMsg("extended_dummy_messages/State").get
  val controlMsg = ROSHelpers.createROSMsg("extended_dummy_messages/Control").get

  val uni2spAttr = ROSHelpers.msgToAttr(stateMsg)
  val sp2uniAttr = ROSHelpers.msgToAttr(controlMsg)

  println(uni2spAttr)
  println(sp2uniAttr)

//  assert(false)

  val t0 = System.nanoTime()
  val model = new Example(system)
  val t1 = System.nanoTime()
  println("Time to make model: " + (t1 - t0) / 1e9d + " seconds")

  val operations = model.operations
  val idables = model.getIDAbles()
  val init = model.getInitialState()
  val resources = model.makeResources()


  idables.find(_.name == "ur.attachOFInMoveit").foreach { o =>
    o.asInstanceOf[Operation].conditions.foreach(c => println(prettyPrint(idables)(c.guard) + " -- " + c.attributes.getAs[String]("kind")))
  }

  assert(false)


  idables.foreach { println }

  println("Initial state")
  init.map { case (id, value) =>
    println(idables.find(_.id == id).get.name + " - " + value.toString)
  }

  println("Resources")
  resources.foreach { println }
}
