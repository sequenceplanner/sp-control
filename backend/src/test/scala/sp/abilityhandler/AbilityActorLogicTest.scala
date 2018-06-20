package sp.abilityhandler

import org.scalatest._
import sp.domain._
import sp.domain.Logic._

/**
  * Created by kristofer on 2017-03-06.
  */
class AbilityActorLogicTest extends FreeSpec with Matchers{
  import sp.abilityhandler.{APIAbilityHandler => api}


  val v1 = Thing("v1")
  val pre = Condition(EQ(v1.id, 1), List(Action(v1.id, ValueHolder(2))))
  val post = Condition(EQ(v1.id, 3), List(Action(v1.id, ValueHolder(4))))
  val started = Condition(EQ(v1.id, 2), List())
  val reset = Condition(AlwaysTrue, List(Action(v1.id, ValueHolder(1))))
  val ab = api.Ability(
    "test",
    ID.newID,
    pre,
    started,
    post,
    reset,
    List(),
    List(),
    SPAttributes(
      "syncedExecution" -> true,
      "syncedFinished" -> false
    )
  )


  "Methods tests" - {

    "extractVariables" in {
      val logic = new AbilityActorLogic {
        override val ability = APIAbilityHandler.Ability("test", ID.newID)
      }
      val v1 = Thing("v1")
      val v2 = Thing("v2")
      val v3 = Thing("v3")
      val v4 = Thing("v4")

      val g = AND(List(OR(List(EQ(v1.id, 3), EQ(v4.id, 40))), NEQ(v2.id, ValueHolder(false))))
      val a = Action(v3.id, ValueHolder(2))

      val res = logic.extractVariableIDs(Condition(g, List(a)))
      res.toSet shouldEqual Set(v1.id, v2.id, v3.id, v4.id)
    }



    import AbilityStatus._

    "printing state changes" in {
      val logic = new AbilityActorLogic {
        override val ability = ab.copy(attributes = SPAttributes())
      }
      println(logic.state)
      println("ev: " + logic.evalState(Map(v1.id -> 0)))
      println(logic.state)
      println("ev: " + logic.evalState(Map(v1.id -> 3)))
      println(logic.state)
      println("ev: " + logic.evalState(Map(v1.id -> 1)))
      println(logic.state)
      println("ev: " + logic.evalState(Map(v1.id -> 1), "start"))
      println(logic.state)
      println("ev: " + logic.evalState(Map(v1.id -> 2)))
      println(logic.state)
      println("ev: " + logic.evalState(Map(v1.id -> 3)))
      println(logic.state)
      println("ev: " + logic.evalState(Map(v1.id -> 4)))
      println(logic.state)

    }



    "test simple state machine " in {
      val logic = new AbilityActorLogic {
        override val ability = ab.copy(attributes = SPAttributes())
      }
      logic.state shouldEqual Unavailable
      logic.evalState(Map(v1.id -> 0))._1 shouldEqual Some(NotEnabled)
      logic.evalState(Map(v1.id -> 3))._1 shouldEqual None
      logic.evalState(Map(v1.id -> 1))._1 shouldEqual Some(Enabled)
      logic.evalState(Map(v1.id -> 1), "start")._1 shouldEqual Some(Starting)
      logic.evalState(Map(v1.id -> 2))._1 shouldEqual Some(Executing)
      logic.evalState(Map(v1.id -> 3))._1 shouldEqual Some(Finished)
      // Auto restart
      logic.evalState(Map(v1.id -> 4))._1 shouldEqual Some(Enabled)
    }

    "test state machine with well defined executing" in {
      val logic = new AbilityActorLogic {
        override val ability = ab
      }
      logic.state shouldEqual Unavailable
      logic.evalState(Map(v1.id -> 0))._1 shouldEqual Some(NotEnabled)
      logic.evalState(Map(v1.id -> 3))._1 shouldEqual None
      logic.evalState(Map(v1.id -> 2))._1 shouldEqual Some(Executing)
      logic.evalState(Map(v1.id -> 3))._1 shouldEqual Some(Finished)
    }


    "test state machine with well defined finished" in {
      val logic = new AbilityActorLogic {
        override val ability = ab.copy(
        resetCondition = Condition(EQ(v1.id, 4), List(Action(v1.id, ValueHolder(1)))),
        attributes = SPAttributes("syncedFinished" -> true))
      }
      logic.state shouldEqual Unavailable
      logic.evalState(Map(v1.id -> 0))._1 shouldEqual Some(NotEnabled)
      logic.evalState(Map(v1.id -> 2))._1 shouldEqual None
      logic.evalState(Map(v1.id -> 3))._1 shouldEqual Some(Finished)
      logic.evalState(Map(v1.id -> 1))._1 shouldEqual None
      logic.evalState(Map(v1.id -> 4))._1 shouldEqual Some(Enabled)
    }


    "Keep unavailible when when missing state ids" in {
      val logic = new AbilityActorLogic {
        override val ability = ab
      }
      logic.state shouldEqual Unavailable
      logic.evalState(Map())._1 shouldEqual None

    }

    "startNReset" in {
      val logic = new AbilityActorLogic {
        override val ability = ab.copy(
          resetCondition = Condition(EQ(v1.id, 4), List(Action(v1.id, ValueHolder(1))))
        )
      }


      logic.state shouldEqual Unavailable
      val init: Map[ID, SPValue] = Map(v1.id -> 0)
      logic.evalState(init)._1 shouldEqual Some(NotEnabled)
      logic.start(init) shouldEqual  None
      logic.start(Map(v1.id -> 1)) shouldEqual Some(Map(v1.id -> SPValue(2)))
      logic.state shouldEqual Starting
      logic.evalState(Map(v1.id -> 2))._1 shouldEqual Some(Executing)
      logic.reset(Map(v1.id -> 2)) shouldEqual None
      logic.state shouldEqual ForcedReset
      logic.evalState(Map(v1.id -> 2))._2 shouldEqual Some(Map(v1.id -> SPValue(1)))
      logic.state shouldEqual Enabled
      logic.evalState(Map(v1.id -> 10))
      logic.state shouldEqual NotEnabled
    }


    "sendCmc" in {
      val logic = new AbilityActorLogic {
        override val ability = ab
      }

      logic.start(Map(v1.id -> 0)) shouldEqual None
      logic.start(Map(v1.id -> 1)) shouldEqual Some(Map(v1.id -> SPValue(2)))

      logic.state = Starting

      logic.start(Map(v1.id -> 1)) shouldEqual None


    }





  }

  "Ability logic test" - {
    "check force attributes" in {
      val logic = new AbilityLogic {}

      assert(logic.syncedExecution(ab))
      assert(!logic.syncedFinished(ab))

    }
  }
}
