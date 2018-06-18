package sp.modelSupport

import org.scalatest._
import sp.domain.Logic._
import sp.domain._


class Model extends ModelDSL {
  v("a", "off", List("off", "on"))
  v("b", "off", List("off", "on"))

  o("doA",
    c("pre", "a==off", "a := on"),
    c("post", "a==on", "a := off"))

  o("doB",
    c("pre", "b==off", "b := on"),
    c("post", "b==on", "b := off"))

  x("test", "b==on && a==on")


  runner("runner", initState = Map())
}



class SynthesisTest extends FreeSpec with Matchers {
  println
  println



  val t = new Model

  t.mes.foreach { println }

  println
  println

  val idables = t.buildModel()
  println
  println
  println
  println
  idables.foreach { println }



  println
  println

  // try {
  // val (ops,_,_) = SynthesizeModel.synthesizeModel(idables)
  // println
  // println
  // println
  // println

  //   println(ops)
  // } catch {
  //   case e:Throwable =>
  //     e.printStackTrace
  // }

}
