package sp.patrikmodel.modeledCases

import sp.patrikmodel.CollectorModel
import sp.domain._
import sp.domain.Logic._

/**
 * Created by patrik on 2015-11-11.
 */
case class SimpleTest(modelName: String = "Simple test") extends CollectorModel {
  val hatt = SPAttributes("hierarchy" -> Set(modelName))
  v(name = "vCounter", idleValue = "0", domain = Seq("0", "1", "2", "3", "4"), attributes = hatt)

  op("first", c("vCounter", "0", "1", "2") merge hatt)

  op("second", c("vCounter", "2", "3", "0") merge hatt)

  val preg = Set("vCounter == 0")
  val prea = Set("vCounter = 4")
  val postg = Set("vCounter == 4")
  val posta = Set("vCounter = 0")

  val thirdattr = SPAttributes("preGuard" -> preg, "preAction" -> prea,
    "postGuard" -> postg, "postAction" -> posta)
  op("third", thirdattr merge hatt)

  v(name = "vOther", idleValue = "0", domain = Seq("0", "1"), attributes = hatt)

  x("forbidden1", "vCounter == 1 && vOther == 1", attributes = hatt)
}
