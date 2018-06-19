package spgui.widgets.sopmaker

import generators.{RNG, SopGeneration}
import sp.domain._

object ExampleSops {
  val ops = List(
    Operation("op1"),
    Operation("op2"),
    Operation("op3"),
    Operation("op4"),
    Operation("op5"),
    Operation("op6"),
    Operation("op7"),
    Operation("op8"),
    Operation("op9"),
    Operation("op10"),
    Operation("op11"),
    Operation("test")
  )

  def tinySop = Sequence( List(SOP(ops(0)), SOP(ops(1))))

  def giantSop = {
    val (sop, _) = SopGeneration.sop(RNG.Simple(12345))
    println(SopGeneration.showSop(sop))
    sop
  }

  def megaSop = Sequence(List(
    Parallel(List(
      giantSop, giantSop, giantSop, giantSop
    )),
    giantSop,
    giantSop
  ))
}
