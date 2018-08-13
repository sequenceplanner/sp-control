package sp

import org.scalatest._
import sp.domain._
import sp.domain.Logic._



trait ExampleModelForTesting {
  val v1 = Thing("v1", SPAttributes("domain"-> List("foo", "bar")))
  val v2 = Thing("v2", SPAttributes("domain"-> List(true, false)))

  val conditionForO1 = Condition(AND(List(EQ(SVIDEval(v1.id), ValueHolder("foo")), NEQ(SVIDEval(v2.id), ValueHolder(true)))))
  val o1 = Operation("o1", List(conditionForO1))

  val conditionForO2 = Condition(EQ(SVIDEval(o1.id), ValueHolder("e")))
  val o2 = Operation("o2", List(conditionForO2))

  val r1 = Thing("r1", SPAttributes("someAttr" ->"kalle"))

  val sop = SOPSpec("sop", List(SOP(Sequence(List(SOP(o1), SOP(o2))))))

  val struct = Struct("struct",
    makeStructNodes(
      r1.children(
        o1.children(
          v1, v2
        ),
        o2.children(
          v2
        )
      )
    )
  )


  val model = List(v1, v2, o1, o2, r1, sop, struct)
}

class ModelDSLTesting extends FreeSpec with Matchers with ExampleModelForTesting {

  // these are possible use cases

  m.items  // gives me a list of all items

  m.o1.conditions  // gives me a list of conditions

  m.struct.r1.o1.v1.id  // should give me the id of v1

  // making conditions like this
  Condition(v1 == "bar" && v2, v2 := false)

  m.r1.someAttr  // gives me kalle
  m.r1.someAttr.value.kalle   // maybe this? Then i can see the actual value in intellij without running it. However we can maybe use scala worksheet or repl instead


  // Then when we have this, we can make specific DSLs for the SPControl where we generate special things for drivers etc.

}
