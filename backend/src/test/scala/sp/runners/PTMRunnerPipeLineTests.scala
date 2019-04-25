package sp.runners

import akka.actor.ActorSystem
import akka.pattern
import akka.stream._
import akka.stream.scaladsl._
import akka.stream.testkit.scaladsl._
import akka.testkit._
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, FreeSpecLike, Matchers}
import sp.domain._
import sp.domain.Logic._


import scala.concurrent._
import scala.concurrent.duration._
import scala.util._

import PTM_Models._


trait SimplePlanningModel extends ConditionParseSupport {
  val p1 = Thing("p1")  // all has domain {none, partA, partB}
  val p2 = Thing("p2")
  val p3 = Thing("p3")
  val gripper = Thing("gripper")
  val ids = List(p1, p2, p3, gripper)

  val t = SPValue(true)
  val f = SPValue(false)
  val none = SPValue("none")
  val partA = SPValue("partA")
  val partB = SPValue("partB")

  def hasPred(t: Thing, v: SPValue): Proposition = EQ(SVIDEval(t.id), ValueHolder(v))
  def notHavePred(t: Thing, v: SPValue): Proposition = NEQ(SVIDEval(t.id), ValueHolder(v))
  def move(from: Thing, to: Thing): List[Action] = List(Action(to.id, ASSIGN(from.id)), Action(from.id, ValueHolder(none)))


  def makePlace(pos: Thing) = {
    val enabled = StatePredicate("enabled", AND(List(hasPred(pos, none), notHavePred(gripper, none))))
    val run = PTMTransition(Condition(enabled.predicate, move(gripper, pos)))
    PTMOperation(List(enabled), List(run), List(), List(), Operation(s"PlaceAt_${pos.name}"))
  }
  def makePick(pos: Thing) = {
    val enabled = StatePredicate("enabled", AND(List(notHavePred(pos, none), hasPred(gripper, none))))
    val run = PTMTransition(Condition(enabled.predicate, move(pos, gripper)))
    PTMOperation(List(enabled), List(run), List(), List(), Operation(s"PickAt_${pos.name}"))
  }

  val abilities = List(p1, p2, p3).flatMap(p => List(makePick(p), makePlace(p)))


//  def makeMoveOP(name: String, start: Condition, goal: Condition) = {
//    val variable = Thing(name)
//    val init = StatePredicate("i", EQ(SVIDEval(variable.id), ValueHolder("i")))
//    val execute = StatePredicate("e", EQ(SVIDEval(variable.id), ValueHolder("e")))
//    val pre = PTMTransition(
//      start.copy(
//        guard = AND(List(init.predicate, start.guard)),
//        action = Action(variable.id, ValueHolder("e")) +: start.action,
//        attributes = start.attributes + ("kind" -> "pre")
//        ),
//      "pre"
//    )
//    val post = PTMTransition(
//      goal.copy(
//        guard = AND(List(execute.predicate, goal.guard)),
//        action = Action(variable.id, ValueHolder("f")) +: goal.action,
//        attributes = goal.attributes + ("kind" -> "post")
//      ),
//      "post"
//    )
//    (PTMOperation(List(init, execute), List(pre), List(post), List(), Operation("OP_"+name)), variable)
//  }

  val from12To23 = (
    "from12To23",
    parseGuard("p1 == partA && p2 == partB && p3 == none", ids),
    parseGuard("p1 == none && p2 == partA && p3 == partB", ids)
  )

  val from23To13 = (
    "from23To13",
    parseGuard("p1 == none && p2 == partA && p3 == partB", ids),
    parseGuard("p1 == partB && p2 == none && p3 == partA", ids)
  )

  val from12ToImpossible = (
    "from12ToImpossible",
    parseGuard("p1 == partA && p2 == partB && p3 == none", ids),
    parseGuard("p1 == none && p2 == none && p3 == none", ids)
  )

  val opsMove = List(from12To23, from23To13).map{case (name, start, goal) =>
    makePlanningOP(name, Condition(start), Condition(goal))
  }

  val opsSingle = List(from12To23).map{case (name, start, goal) =>
    makePlanningOP(name, Condition(start), Condition(goal))
  }

  val opsImpossible = List(from12ToImpossible).map{case (name, start, goal) =>
    makePlanningOP(name, Condition(start), Condition(goal))
  }

  val allOpsVars = List(opsImpossible, opsMove, opsSingle).flatMap(_.map(_._2))
  val opsMap = allOpsVars.map(_.id -> SPValue("i"))

  val initState = SPState("runner",
    Map(
      p1.id -> partA,
      p2.id -> partB,
      p3.id -> none,
      gripper.id -> none
    ) ++ opsMap
  )



}

class PTMRunnerPipeLineTests(_system: ActorSystem) extends TestKit(_system) with ImplicitSender
  with FreeSpecLike with Matchers with BeforeAndAfterAll with SimplePlanningModel {


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
  implicit val askTimeout = Timeout(1 seconds)
  import akka.pattern.ask

  "PTM runner actor test" - {
    val m = new TestModel {}

    "simple change internals" in {
      var init = PTMRunnerState(
        state = Some(SPState(state = Map(m.v1.id -> f, m.v2.id -> f, m.v3.id -> f, m.v4.id -> f))), // replace complete state
        ops = List(m.o1),
        abs = List(m.o2),
        opsQ = List(m.preO1.id),
        absQ = List(m.preO2.id),
        pause = Some(false)
      )

      val runner = system.actorOf(PTMRunnerActor.props(init))

      var answer = runner ? SPState(state = Map(m.v1.id -> f, m.v2.id -> f, m.v3.id -> f, m.v4.id -> f))
      val result = Await.result(answer, 1.seconds)

      init = PTMRunnerState(
        state = Some(SPState(state = Map(m.v1.id -> f, m.v2.id -> f, m.v3.id -> f, m.v4.id -> f))), // replace complete state
        ops = List(m.o1, m.o2),
        abs = List(),
        opsQ = List(m.preO1.id),
        absQ = List(),
        pause = Some(false)
      )

      runner ! init
      answer = runner ? SPState(state = Map(m.v1.id -> f, m.v2.id -> f, m.v3.id -> f, m.v4.id -> f))
      println(Await.result(answer, 1.seconds))

      init = PTMRunnerState(
        state = Some(SPState(state = Map(m.v1.id -> f, m.v2.id -> f, m.v3.id -> f, m.v4.id -> f))), // replace complete state
        ops = List(),
        abs = List(m.o1, m.o2),
        opsQ = List(),
        absQ = List(m.preO1.id, m.preO2.id),
        pause = Some(false)
      )

      runner ! init
      answer = runner ? SPState(state = Map(m.v1.id -> f, m.v2.id -> f, m.v3.id -> f, m.v4.id -> f))
      println(Await.result(answer, 1.seconds))

    }

    "testing the model" in {
      var res = abilities.filter(_.controlled.exists(x => x.condition.eval(initState)))
      res.map(_.o.name) shouldEqual List("PickAt_p1", "PickAt_p2")

      res = opsMove.map(_._1).filter(_.controlled.filter(x => x.condition.eval(initState)).nonEmpty)
      res.map(_.o.name) shouldEqual List("OP_from12To23")

      var newState = initState.next(Map(gripper.id -> partA, p1.id -> none))
      res = abilities.filter(_.controlled.exists(x => x.condition.eval(newState)))
      res.map(_.o.name).foreach(println)
      res.map(_.o.name) shouldEqual List("PlaceAt_p1", "PlaceAt_p3")



    }

    "test evaluate predicates" in {
      val ops = opsMove.map(_._1)
      val predMap = evaluatePredicates(initState, ops)
      val res = predMap.flatMap(_._2.map(_.name)).toList
      res shouldEqual List("i", "i")
    }

    "test goal making" in {
      val from12To23 = opsMove(0)
      val from23To13 = opsMove(1)
      val ops = opsMove.map(_._1)
      val predMap = evaluatePredicates(initState, ops)
      val noGoal = makeGoal(predMap)
      noGoal shouldEqual AND(List())

      val aGoal = makeGoal(evaluatePredicates(initState.next(from12To23._2.id -> SPValue("e")), ops))
      println(aGoal)
      aGoal shouldEqual AND(List(from12To23._1.unControlled.head.condition.guard))

      val twoGoals = makeGoal(evaluatePredicates(initState.next(Map(
        from12To23._2.id -> SPValue("e"),
        from23To13._2.id -> SPValue("e")
      )), ops))
      println(twoGoals)
      twoGoals shouldEqual AND(List(from12To23._1.unControlled.head.condition.guard, from23To13._1.unControlled.head.condition.guard))
    }

    "testing planning" in {
      //val goal =
    }

  }


  // "testing runner pipeline" - {
  //   "Run simple sequence" in {
  //     val test = new SimpleSequence with OperationRunnerTransitionsNoReset{}
  //     val pipe = RunnerPipeline(
  //       operations = test.ops,
  //       transitionSystem = test.transitions,
  //       initialState = test.initialState,
  //       name = "testRunner",
  //       system = system
  //     )


  //     val s = Source(1 to 6).map(x =>  StateUpd(SPState("tick", Map()), List()))
  //     val future = s.via(pipe.runnerFlow).runWith(Sink.fold(List[SPState]())(_ :+ _))
  //     val result = Await.result(future, 1.seconds)
  //     //result.foreach(println)

  //     assert(result.last.get(test.o3.id).contains(SPValue(test.finished)))
  //   }

  //   "No ticker when no ticker" in {
  //     val test = new SimpleSequence with OperationRunnerTransitionsNoReset{}
  //     val pipe = RunnerPipeline(
  //       operations = test.ops,
  //       transitionSystem = test.transitions,
  //       initialState = test.initialState,
  //       name = "testRunner",
  //       system = system
  //     )


  //     val s = Source.empty[StateUpd]
  //     val future = s.via(pipe.runnerFlow).runWith(Sink.fold(List[SPState]())(_ :+ _))
  //     val result = Await.result(future, 1.seconds)

  //     assert(result.isEmpty)
  //   }

  //   "Run simple sequence with Ticker" in {
  //     val test = new SimpleSequence with OperationRunnerTransitionsNoReset{}
  //     val pipe = RunnerPipeline(
  //       operations = test.ops,
  //       transitionSystem = test.transitions,
  //       initialState = test.initialState,
  //       name = "testRunner",
  //       system = system
  //     )


  //     val s = Source.empty[StateUpd]
  //     val res = s
  //       .via(pipe.runnerFlow)
  //       .runWith(TestSink.probe[SPState])
  //       .request(10)
  //       .receiveWhile(1 seconds){
  //         case x: OnNext[SPState] if x.element.get(test.o3.id).contains(SPValue(test.finished)) => true
  //         case x: OnNext[_]  => false
  //       }

  //     println(res)

  //     assert(res.exists(x => x))
  //   }

  //   "Run sequence with auto reset with Ticker" in {
  //     val test = new SimpleSequence with OperationRunnerTransitionsWithAutoReset {}
  //     val pipe = RunnerPipeline(
  //       operations = test.ops,
  //       transitionSystem = test.transitions,
  //       initialState = test.initialState,
  //       name = "testRunner",
  //       system = system
  //     )


  //     val s = Source.empty[StateUpd]
  //     val res = s
  //       .via(pipe.runnerFlow)
  //       .runWith(TestSink.probe[SPState])
  //       .request(100)
  //       .receiveWhile(1 seconds){
  //         case x: OnNext[SPState] if x.element.get(test.o3.id).contains(SPValue(test.finished)) => true
  //         case x: OnNext[_]  => false
  //       }


  //     println(res)

  //     assert(res.exists(x => x) && !res.last)
  //   }

  //   "Run sequence with start events and no Ticker" in {
  //     val test = new SimpleSequence with OperationRunnerWithStartEvents {}
  //     val pipe = RunnerPipeline(
  //       operations = test.ops,
  //       transitionSystem = test.transitions,
  //       initialState = test.initialState,
  //       name = "testRunner",
  //       system = system
  //     )


  //     val s = Source.queue[StateUpd](10, akka.stream.OverflowStrategy.backpressure)
  //     val res = s
  //       .via(pipe.runnerFlow)
  //       .toMat(Sink.fold(List[SPState]())(_ :+ _))(Keep.both)
  //       .run()


  //     res._1.offer(StateUpd(SPState("test1", Map()), List())).foreach(println)(system.dispatcher)
  //     res._1.offer(StateUpd(SPState("test1", Map()), List(FireEvent("start", test.o1.id)))).foreach(println)(system.dispatcher)
  //     res._1.offer(StateUpd(SPState("test1", Map()), List())).foreach(println)(system.dispatcher)
  //     res._1.offer(StateUpd(SPState("test1", Map()), List())).foreach(println)(system.dispatcher)
  //     res._1.offer(StateUpd(SPState("test1", Map()), List(FireEvent("start", test.o2.id)))).foreach(println)(system.dispatcher)
  //     res._1.offer(StateUpd(SPState("test1", Map()), List(FireEvent("start", test.o3.id)))).foreach(println)(system.dispatcher)
  //     res._1.offer(StateUpd(SPState("test1", Map()), List())).foreach(println)(system.dispatcher)

  //     res._1.complete()

  //     val result = Await.result(res._2, 5.seconds)
  //     println(result)
  //     assert(result.last.get(test.o3.id).contains(SPValue(test.finished)))
  //   }

  //   "Run sequence with start events but not in correct state" in {
  //     val test = new SimpleSequence with OperationRunnerWithStartEvents {}
  //     val pipe = RunnerPipeline(
  //       operations = test.ops,
  //       transitionSystem = test.transitions,
  //       initialState = test.initialState,
  //       name = "testRunner",
  //       system = system
  //     )


  //     val s = Source.queue[StateUpd](10, akka.stream.OverflowStrategy.backpressure)
  //     val res = s
  //       .via(pipe.runnerFlow)
  //       .toMat(Sink.fold(List[SPState]())(_ :+ _))(Keep.both)
  //       .run()


  //     res._1.offer(StateUpd(SPState("test1", Map()), List(FireEvent("start", test.o2.id)))).foreach(println)(system.dispatcher)
  //     res._1.offer(StateUpd(SPState("test1", Map()), List(FireEvent("start", test.o1.id)))).foreach(println)(system.dispatcher)
  //     res._1.offer(StateUpd(SPState("test1", Map()), List(FireEvent("start", test.o3.id)))).foreach(println)(system.dispatcher)
  //     res._1.offer(StateUpd(SPState("test1", Map()), List())).foreach(println)(system.dispatcher)
  //     res._1.offer(StateUpd(SPState("test1", Map()), List())).foreach(println)(system.dispatcher)
  //     res._1.offer(StateUpd(SPState("test1", Map()), List())).foreach(println)(system.dispatcher)

  //     res._1.complete()

  //     val result = Await.result(res._2, 5.seconds)
  //     println(result)
  //     assert(!result.last.get(test.o3.id).contains(SPValue(test.finished)))
  //   }


  // }





//
//  "strict collection" in {
//    //#strict-collection
//    val sinkUnderTest = Flow[Int].map(_ * 2).toMat(Sink.fold(0)(_ + _))(Keep.right)
//
//    val future = Source(1 to 4).runWith(sinkUnderTest)
//    val result = Await.result(future, 3.seconds)
//    assert(result == 20)
//    //#strict-collection
//  }
//
//  "grouped part of infinite stream" in {
//    //#grouped-infinite
//
//    val sourceUnderTest = Source.repeat(1).map(_ * 2)
//
//    val future = sourceUnderTest.take(10).runWith(Sink.seq)
//    val result = Await.result(future, 3.seconds)
//    assert(result == Seq.fill(10)(2))
//    //#grouped-infinite
//  }
//
//  "folded stream" in {
//    //#folded-stream
//    val flowUnderTest = Flow[Int].takeWhile(_ < 5)
//
//    val future = Source(1 to 10).via(flowUnderTest).runWith(Sink.fold(Seq.empty[Int])(_ :+ _))
//    val result = Await.result(future, 3.seconds)
//    assert(result == (1 to 4))
//    //#folded-stream
//  }
//
//  "pipe to test probe" in {
//    //#pipeto-testprobe
//    import akka.pattern.pipe
//    import system.dispatcher
//
//    val sourceUnderTest = Source(1 to 4).grouped(2)
//
//    val probe = TestProbe()
//    sourceUnderTest.runWith(Sink.seq).pipeTo(probe.ref)
//    probe.expectMsg(3.seconds, Seq(Seq(1, 2), Seq(3, 4)))
//    //#pipeto-testprobe
//  }
//
//  "sink actor ref" in {
//    //#sink-actorref
//    case object Tick
//    val sourceUnderTest = Source.tick(0.seconds, 200.millis, Tick)
//
//    val probe = TestProbe()
//    val cancellable = sourceUnderTest.to(Sink.actorRef(probe.ref, "completed")).run()
//
//    probe.expectMsg(1.second, Tick)
//    probe.expectNoMsg(100.millis)
//    probe.expectMsg(3.seconds, Tick)
//    cancellable.cancel()
//    probe.expectMsg(3.seconds, "completed")
//    //#sink-actorref
//  }
//
//  "source actor ref" in {
//    //#source-actorref
//    val sinkUnderTest = Flow[Int].map(_.toString).toMat(Sink.fold("")(_ + _))(Keep.right)
//
//    val (ref, future) = Source.actorRef(8, OverflowStrategy.fail)
//      .toMat(sinkUnderTest)(Keep.both).run()
//
//    ref ! 1
//    ref ! 2
//    ref ! 3
//    ref ! akka.actor.Status.Success("done")
//
//    val result = Await.result(future, 3.seconds)
//    assert(result == "123")
//    //#source-actorref
//  }
//
//  "test sink probe" in {
//    //#test-sink-probe
//    val sourceUnderTest = Source(1 to 4).filter(_ % 2 == 0).map(_ * 2)
//
//    sourceUnderTest
//      .runWith(TestSink.probe[Int])
//      .request(2)
//      .expectNext(4, 8)
//      .expectComplete()
//    //#test-sink-probe
//  }
//
//  "test source probe" in {
//    //#test-source-probe
//    val sinkUnderTest = Sink.cancelled
//
//    TestSource.probe[Int]
//      .toMat(sinkUnderTest)(Keep.left)
//      .run()
//      .expectCancellation()
//    //#test-source-probe
//  }
//
//  "injecting failure" in {
//    //#injecting-failure
//    val sinkUnderTest = Sink.head[Int]
//
//    val (probe, future) = TestSource.probe[Int]
//      .toMat(sinkUnderTest)(Keep.both)
//      .run()
//    probe.sendError(new Exception("boom"))
//
//    Await.ready(future, 3.seconds)
//    val Failure(exception) = future.value.get
//    assert(exception.getMessage == "boom")
//    //#injecting-failure
//  }
//
//  "test source and a sink" in {
//    import system.dispatcher
//    //#test-source-and-sink
//    val flowUnderTest = Flow[Int].mapAsyncUnordered(2) { sleep ⇒
//      pattern.after(10.millis * sleep, using = system.scheduler)(Future.successful(sleep))
//    }
//
//    val (pub, sub) = TestSource.probe[Int]
//      .via(flowUnderTest)
//      .toMat(TestSink.probe[Int])(Keep.both)
//      .run()
//
//    sub.request(n = 3)
//    pub.sendNext(3)
//    pub.sendNext(2)
//    pub.sendNext(1)
//    sub.expectNextUnordered(1, 2, 3)
//
//    pub.sendError(new Exception("Power surge in the linear subroutine C-47!"))
//    val ex = sub.expectError()
//    assert(ex.getMessage.contains("C-47"))
//    //#test-source-and-sink
//  }

}
