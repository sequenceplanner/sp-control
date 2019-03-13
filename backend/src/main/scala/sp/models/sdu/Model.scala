// package sp.models.sdu

// import scala.concurrent.duration._

// import akka.stream._
// import akka.stream.scaladsl._
// import akka.{ NotUsed, Done }

// import akka.actor._

// import sp.domain.Logic._
// import sp.domain._

// import sp.modelSupport._
// import sp.modelSupport.opcua._
// import sp.modelSupport.ros2._


// object UR {
//   val initialState = "Unknown"

//   val poses = List("Unknown", "above_station",
//     "above_blue", "at_blue", "above_place_blue", "place_blue",
//     "above_red", "at_red", "above_place_red", "place_red",
//     "above_yellow", "at_yellow", "above_place_yellow", "place_yellow",
//     "above_green", "at_green", "above_place_green", "place_green"
//   )
// }


// class UR(override val system: ActorSystem) extends OPCUAResource {
//   val url = "opc.tcp://localhost:12686"

//   // inputs
//   val spindle_end = i("UR10_PNP_SpindleGear_end", false)
//   val gearwheel_end = i("UR10_PNP_GearWheel_end", false)
//   val metalbush_end = i("UR10_PNP_MetalBush_end", false)
//   val pnp_end = i("UR10_PNP_Op_end", false)

//   // outputs
//   val spindle_start = o("UR10_PNP_SpindleGear_start", false)
//   val gearwheel_start = o("UR10_PNP_GearWheel_start", false)
//   val metalbush_start = o("UR10_PNP_MetalBush_start", false)
//   val pnp_start = o("UR10_PNP_Op_start", false)

//   val inputs = List(spindle_end, gearwheel_end, metalbush_end, pnp_end)
//   val outputs = List(spindle_start, gearwheel_start, metalbush_start, pnp_start)

//   val subIdents = inputs.flatMap(i => things.find(_.id == i).map(_.name))
//   val subMap = inputs.flatMap(i => things.find(_.id == i).map(_.name -> i)).toMap
//   val subMapping = stringToIDMapper(subMap)

//   subscribe(subIdents, 100, subMapping)

//   val pubIdents = outputs.flatMap(i => things.find(_.id == i).map(_.name))
//   val pubMap = outputs.flatMap(i => things.find(_.id == i).map(i -> _.name)).toMap
//   val pubMapping = IDToStringMapper(pubMap)

//   publish(pubIdents, Some(1000.millis), pubMapping)

//   a("spindle")(
//     c("pre", "!UR10_PNP_SpindleGear_start && !UR10_PNP_SpindleGear_end", "UR10_PNP_SpindleGear_start := true"),
//     c("started", "UR10_PNP_SpindleGear_start && !UR10_PNP_SpindleGear_end"),
//     c("post", "UR10_PNP_SpindleGear_end == true", "UR10_PNP_SpindleGear_start := false"),
//     c("reset", "true"))

//   a("gearwheel")(
//     c("pre", "!UR10_PNP_GearWheel_start && !UR10_PNP_GearWheel_end", "UR10_PNP_GearWheel_start := true"),
//     c("started", "UR10_PNP_GearWheel_start && !UR10_PNP_GearWheel_end"),
//     c("post", "UR10_PNP_GearWheel_end == true", "UR10_PNP_GearWheel_start := false"),
//     c("reset", "true"))

//   a("metalbush")(
//     c("pre", "!UR10_PNP_MetalBush_start && !UR10_PNP_MetalBush_end", "UR10_PNP_MetalBush_start := true"),
//     c("started", "UR10_PNP_MetalBush_start && !UR10_PNP_MetalBush_end"),
//     c("post", "UR10_PNP_MetalBush_end == true", "UR10_PNP_MetalBush_start := false"),
//     c("reset", "true"))

//   a("pnp")(
//     c("pre", "!UR10_PNP_Op_start && !UR10_PNP_Op_end", "UR10_PNP_Op_start := true"),
//     c("started", "UR10_PNP_Op_start && !UR10_PNP_Op_end"),
//     c("post", "UR10_PNP_Op_end == true", "UR10_PNP_Op_start := false"),
//     c("reset", "true"))
// }

// class Human(override val system: ActorSystem) extends ROSResource {
//   val hasBlue = i("hasBlue", false)
//   val hasRed = i("hasRed", false)
//   val hasYellow = i("hasYellow", false)
//   val hasGreen = i("hasGreen", false)

//   val nameMap = Map("hasBlue" -> hasBlue, "hasRed" -> hasRed, "hasYellow" -> hasYellow, "hasGreen" -> hasGreen)

//   val subMapping = Flow[Map[String, SPValue]].mapConcat{ msg =>
//     msg.flatMap {
//       case ("data", spv) =>
//         spv.to[String].toOption.map { str =>
//           val a = str.split(":").flatMap{ color =>
//             val x = color.split("=")
//             for {
//               name <- x.lift(0)
//               v <- x.lift(1)
//               id <- nameMap.get(name)
//             } yield {
//               id -> SPValue(v.equals("true"))
//             }
//           }
//           a.toMap
//         }

//       case _ => None
//     }
//   }
//   subscribe("/unification_roscontrol/demo_human", "std_msgs/String", subMapping)
// }

// class Model(override val system: ActorSystem) extends MiniModel {
//   use("ur", new UR(system))
//   use("human", new Human(system))

//   v("activeProduct1", "none", List("none", "plain", "spindled", "gearwheeled", "metalbushed"))
//   v("activeProduct2", "none", List("none", "metalbushed"))

//   val addproduct = o("addProduct")(
//     c("pre", "activeProduct1 == 'none'", "activeProduct1 := 'plain'"),
//     c("post", "true"),
//     c("reset", "true")
//   )

//   val spindle = o("spindle", "ur.spindle")(
//     c("pre", "activeProduct1 == 'plain'"),
//     c("post", "true", "activeProduct1 := 'spindled'"),
//     c("reset", "true")
//   )

//   val gearwheel = o("gearwheel", "ur.gearwheel")(
//     c("pre", "activeProduct1 == 'spindled'"),
//     c("post", "true", "activeProduct1 := 'gearwheeled'"),
//     c("reset", "true")
//   )

//   val metalbush = o("metalbush", "ur.metalbush")(
//     c("pre", "activeProduct1 == 'gearwheeled'"),
//     c("post", "true", "activeProduct1 := 'metalbushed'"),
//     c("reset", "true")
//   )

//   val partToHuman = o("partToHuman")(
//     c("pre", "activeProduct1 == 'metalbushed'"),
//     c("post", "human.hasBlue == true", "activeProduct1 := 'none'", "activeProduct2 := 'metalbushed'"),
//     c("reset", "true")
//   )

//   val humanCompletePart = o("humanCompletePart")(
//     c("pre", "activeProduct2 == 'metalbushed'"),
//     c("post", "human.hasRed == true", "activeProduct2 := 'none'"),
//     c("reset", "true")
//   )

//   val urSequence = Sequence(List(SOP(addproduct), SOP(spindle), SOP(gearwheel), SOP(metalbush)))
//   val humanSequence = Sequence(List(SOP(partToHuman), SOP(humanCompletePart)))

//   sop("Sequence", List(Parallel(List(urSequence, humanSequence))))

//   // synthesis
// //  synthesize()

// }
