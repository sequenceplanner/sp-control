// package sp.models.unification

// import org.scalatest._
// import sp.domain.Logic._
// import sp.domain._
// import sp.runners.APIOperationRunner

// class DummyExampleTest extends FreeSpec with Matchers {
//   println
//   println
//   val idables = DummyExampleExtended().buildModel()
//   println
//   println
//   println
//   println
//   idables.foreach { println }

//   println("DVS: **********")
//   val dvs = idables.collect{case t: Thing if t.attributes.keys.contains("driverName") => t}
//   println(dvs.mkString("\n"))

//   println("VARS: **********")
//   val vs = idables.collect{case t: Thing if t.attributes.keys.contains("domain") => t}
//   println(vs.mkString("\n"))

//   println("Mapping")
//   val mapping: Map[ID, ID] = idables.collect{case t: Thing if t.attributes.keys.contains("runnerID") => t}.
//     map(APIOperationRunner.runnerThingToSetup).headOption.map(_.variableMap).getOrElse(Map())
//   println(mapping)

// }
