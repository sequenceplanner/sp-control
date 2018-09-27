package sp.runners

import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl._
import akka.stream.testkit._
import akka.stream.testkit.scaladsl._

import scala.util._
import scala.concurrent.duration._
import scala.concurrent._
import akka.testkit._
import akka.pattern
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, FreeSpecLike, Matchers}
import sp.domain._
import Logic._
import akka.stream.testkit.TestSubscriber.OnNext
import sp.runners.RunnerLogic.FireEvent







class RunnerPipeLineTests(_system: ActorSystem) extends TestKit(_system) with ImplicitSender
  with FreeSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("SP", ConfigFactory.parseString(
    """
      |
    """.stripMargin)))


  override def beforeAll: Unit = {

  }

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }


  implicit val materializer = ActorMaterializer()


  "testing runner pipeline" - {
    "Run simple sequence" in {
      val test = new SimpleSequence with OperationRunnerTransitionsNoReset{}
      val pipe = RunnerPipeline(
        operations = test.ops,
        transitionSystem = test.transitions,
        initialState = test.initialState,
        name = "testRunner",
        system = system
      )


      val s = Source(1 to 6).map(x =>  StateUpd(SPState("tick", Map()), List()))
      val future = s.via(pipe.runnerFlow()).runWith(Sink.fold(List[SPState]())(_ :+ _))
      val result = Await.result(future, 1.seconds)
      //result.foreach(println)

      assert(result.last.get(test.o3.id).contains(SPValue(test.finished)))
    }

    "No ticker when no ticker" in {
      val test = new SimpleSequence with OperationRunnerTransitionsNoReset{}
      val pipe = RunnerPipeline(
        operations = test.ops,
        transitionSystem = test.transitions,
        initialState = test.initialState,
        name = "testRunner",
        system = system
      )


      val s = Source.empty[StateUpd]
      val future = s.via(pipe.runnerFlow()).runWith(Sink.fold(List[SPState]())(_ :+ _))
      val result = Await.result(future, 1.seconds)

      assert(result.isEmpty)
    }

    "Run simple sequence with Ticker" in {
      val test = new SimpleSequence with OperationRunnerTransitionsNoReset{}
      val pipe = RunnerPipeline(
        operations = test.ops,
        transitionSystem = test.transitions,
        initialState = test.initialState,
        name = "testRunner",
        system = system
      )


      val s = Source.empty[StateUpd]
      val res = s
        .via(pipe.runnerFlow(Some(100 milliseconds)))
        .runWith(TestSink.probe[SPState])
        .request(10)
        .receiveWhile(1 seconds){
          case x: OnNext[SPState] if x.element.get(test.o3.id).contains(SPValue(test.finished)) => true
          case x: OnNext[_]  => false
        }

      println(res)

      assert(res.exists(x => x))
    }

    "Run sequence with auto reset with Ticker" in {
      val test = new SimpleSequence with OperationRunnerTransitionsWithAutoReset {}
      val pipe = RunnerPipeline(
        operations = test.ops,
        transitionSystem = test.transitions,
        initialState = test.initialState,
        name = "testRunner",
        system = system
      )


      val s = Source.empty[StateUpd]
      val res = s
        .via(pipe.runnerFlow(Some(100 milliseconds)))
        .runWith(TestSink.probe[SPState])
        .request(100)
        .receiveWhile(1 seconds){
          case x: OnNext[SPState] if x.element.get(test.o3.id).contains(SPValue(test.finished)) => true
          case x: OnNext[_]  => false
        }


      println(res)

      assert(res.exists(x => x) && !res.last)
    }

    "Run sequence with start events and no Ticker" in {
      val test = new SimpleSequence with OperationRunnerWithStartEvents {}
      val pipe = RunnerPipeline(
        operations = test.ops,
        transitionSystem = test.transitions,
        initialState = test.initialState,
        name = "testRunner",
        system = system
      )


      val s = Source.queue[StateUpd](10, akka.stream.OverflowStrategy.backpressure)
      val res = s
        .via(pipe.runnerFlow(None))
        .toMat(Sink.fold(List[SPState]())(_ :+ _))(Keep.both)
        .run()


      res._1.offer(StateUpd(SPState("test1", Map()), List())).foreach(println)(system.dispatcher)
      res._1.offer(StateUpd(SPState("test1", Map()), List(FireEvent("start", test.o1.id)))).foreach(println)(system.dispatcher)
      res._1.offer(StateUpd(SPState("test1", Map()), List())).foreach(println)(system.dispatcher)
      res._1.offer(StateUpd(SPState("test1", Map()), List())).foreach(println)(system.dispatcher)
      res._1.offer(StateUpd(SPState("test1", Map()), List(FireEvent("start", test.o2.id)))).foreach(println)(system.dispatcher)
      res._1.offer(StateUpd(SPState("test1", Map()), List(FireEvent("start", test.o3.id)))).foreach(println)(system.dispatcher)
      res._1.offer(StateUpd(SPState("test1", Map()), List())).foreach(println)(system.dispatcher)

      res._1.complete()

      val result = Await.result(res._2, 5.seconds)
      println(result)
      assert(result.last.get(test.o3.id).contains(SPValue(test.finished)))
    }

    "Run sequence with start events but not in correct state" in {
      val test = new SimpleSequence with OperationRunnerWithStartEvents {}
      val pipe = RunnerPipeline(
        operations = test.ops,
        transitionSystem = test.transitions,
        initialState = test.initialState,
        name = "testRunner",
        system = system
      )


      val s = Source.queue[StateUpd](10, akka.stream.OverflowStrategy.backpressure)
      val res = s
        .via(pipe.runnerFlow(None))
        .toMat(Sink.fold(List[SPState]())(_ :+ _))(Keep.both)
        .run()


      res._1.offer(StateUpd(SPState("test1", Map()), List(FireEvent("start", test.o2.id)))).foreach(println)(system.dispatcher)
      res._1.offer(StateUpd(SPState("test1", Map()), List(FireEvent("start", test.o1.id)))).foreach(println)(system.dispatcher)
      res._1.offer(StateUpd(SPState("test1", Map()), List(FireEvent("start", test.o3.id)))).foreach(println)(system.dispatcher)
      res._1.offer(StateUpd(SPState("test1", Map()), List())).foreach(println)(system.dispatcher)
      res._1.offer(StateUpd(SPState("test1", Map()), List())).foreach(println)(system.dispatcher)
      res._1.offer(StateUpd(SPState("test1", Map()), List())).foreach(println)(system.dispatcher)

      res._1.complete()

      val result = Await.result(res._2, 5.seconds)
      println(result)
      assert(!result.last.get(test.o3.id).contains(SPValue(test.finished)))
    }


  }






  "strict collection" in {
    //#strict-collection
    val sinkUnderTest = Flow[Int].map(_ * 2).toMat(Sink.fold(0)(_ + _))(Keep.right)

    val future = Source(1 to 4).runWith(sinkUnderTest)
    val result = Await.result(future, 3.seconds)
    assert(result == 20)
    //#strict-collection
  }

  "grouped part of infinite stream" in {
    //#grouped-infinite
    import system.dispatcher
    import akka.pattern.pipe

    val sourceUnderTest = Source.repeat(1).map(_ * 2)

    val future = sourceUnderTest.take(10).runWith(Sink.seq)
    val result = Await.result(future, 3.seconds)
    assert(result == Seq.fill(10)(2))
    //#grouped-infinite
  }

  "folded stream" in {
    //#folded-stream
    val flowUnderTest = Flow[Int].takeWhile(_ < 5)

    val future = Source(1 to 10).via(flowUnderTest).runWith(Sink.fold(Seq.empty[Int])(_ :+ _))
    val result = Await.result(future, 3.seconds)
    assert(result == (1 to 4))
    //#folded-stream
  }

  "pipe to test probe" in {
    //#pipeto-testprobe
    import system.dispatcher
    import akka.pattern.pipe

    val sourceUnderTest = Source(1 to 4).grouped(2)

    val probe = TestProbe()
    sourceUnderTest.runWith(Sink.seq).pipeTo(probe.ref)
    probe.expectMsg(3.seconds, Seq(Seq(1, 2), Seq(3, 4)))
    //#pipeto-testprobe
  }

  "sink actor ref" in {
    //#sink-actorref
    case object Tick
    val sourceUnderTest = Source.tick(0.seconds, 200.millis, Tick)

    val probe = TestProbe()
    val cancellable = sourceUnderTest.to(Sink.actorRef(probe.ref, "completed")).run()

    probe.expectMsg(1.second, Tick)
    probe.expectNoMsg(100.millis)
    probe.expectMsg(3.seconds, Tick)
    cancellable.cancel()
    probe.expectMsg(3.seconds, "completed")
    //#sink-actorref
  }

  "source actor ref" in {
    //#source-actorref
    val sinkUnderTest = Flow[Int].map(_.toString).toMat(Sink.fold("")(_ + _))(Keep.right)

    val (ref, future) = Source.actorRef(8, OverflowStrategy.fail)
      .toMat(sinkUnderTest)(Keep.both).run()

    ref ! 1
    ref ! 2
    ref ! 3
    ref ! akka.actor.Status.Success("done")

    val result = Await.result(future, 3.seconds)
    assert(result == "123")
    //#source-actorref
  }

  "test sink probe" in {
    //#test-sink-probe
    val sourceUnderTest = Source(1 to 4).filter(_ % 2 == 0).map(_ * 2)

    sourceUnderTest
      .runWith(TestSink.probe[Int])
      .request(2)
      .expectNext(4, 8)
      .expectComplete()
    //#test-sink-probe
  }

  "test source probe" in {
    //#test-source-probe
    val sinkUnderTest = Sink.cancelled

    TestSource.probe[Int]
      .toMat(sinkUnderTest)(Keep.left)
      .run()
      .expectCancellation()
    //#test-source-probe
  }

  "injecting failure" in {
    //#injecting-failure
    val sinkUnderTest = Sink.head[Int]

    val (probe, future) = TestSource.probe[Int]
      .toMat(sinkUnderTest)(Keep.both)
      .run()
    probe.sendError(new Exception("boom"))

    Await.ready(future, 3.seconds)
    val Failure(exception) = future.value.get
    assert(exception.getMessage == "boom")
    //#injecting-failure
  }

  "test source and a sink" in {
    import system.dispatcher
    //#test-source-and-sink
    val flowUnderTest = Flow[Int].mapAsyncUnordered(2) { sleep ⇒
      pattern.after(10.millis * sleep, using = system.scheduler)(Future.successful(sleep))
    }

    val (pub, sub) = TestSource.probe[Int]
      .via(flowUnderTest)
      .toMat(TestSink.probe[Int])(Keep.both)
      .run()

    sub.request(n = 3)
    pub.sendNext(3)
    pub.sendNext(2)
    pub.sendNext(1)
    sub.expectNextUnordered(1, 2, 3)

    pub.sendError(new Exception("Power surge in the linear subroutine C-47!"))
    val ex = sub.expectError()
    assert(ex.getMessage.contains("C-47"))
    //#test-source-and-sink
  }

}

