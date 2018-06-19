package generators

import sp.domain._
import RNG.Rand

object SopGeneration {
  val operation: Rand[Operation] = RNG.double andThen { case (d, r) => (Operation(d.toString), r) }

  // Sequence of operations generator
  def sop(explosionFactor: Int = 8): Rand[SOP] = rng => SOPHelpers.buildSop(explosionFactor)(EmptySOP, rng)

  val sop: Rand[SOP] = sop()

  implicit class SopWithSize(sop: SOP) {
    def size: Int = {
      if (sop.sop.isEmpty) 1
      else 1 + sop.sop.map(s => s.size).sum
    }
  }

  def showSop(sop: SOP): String = {
    def indent(depth: Int): String = List.fill(depth)("  ").foldLeft("")(_ + _)

    def show(sop: SOP, depth: Int): String = {
      val name = sop match {
        case _: Parallel => "Parallel"
        case _: Alternative => "Alternative"
        case _: Arbitrary => "Arbitrary"
        case _: Sequence => "Sequence"
        case _: SometimeSequence => "SometimeSequence"
        case _: Other => "Other"
        case _: OperationNode => "OperationNode"
        case _ => "EmnptySOP"
      }

      val r1 = s"${indent(depth)}$name\n"
      val rest = sop.sop.map(s => show(s, depth + 1)).foldLeft("")(_ + _)
      r1 + rest
    }

    show(sop, 0)
  }
}

private object SOPHelpers {
  import SopGeneration.SopWithSize

  def wrapSop(s: (SOP, RNG)): (SOP, RNG) = {
    val (sop, rng) = s
    val (res, r) = RNG.double(rng)
    if (sop == EmptySOP) (EmptySOP, r)
    else {
      val list = List(sop)

      val newSop = res match {
        case x if x < 1/3 => Parallel(list)
        case x if x < 2/3 && x >= 1/3 => Sequence(list)
        case x if x < 3/3 && x >= 2/3 => SOP(list)
      }

      (newSop, r)
    }
  }

  def childSop(allowedSteps: Int, childCountRange: Rand[Int])(s: (SOP, RNG)): (SOP, RNG) = {
    if (allowedSteps <= 0) s
    else {

      val (sop, rng) = s

      if (sop == EmptySOP) makeSop(rng)
      else {

        val (childCount, r2) = childCountRange(rng)
        val firstChild = buildSop(allowedSteps)(makeSop(r2))
        val builtSops = (0 to childCount).scanLeft(firstChild) { case ((_, _r), _) =>
          buildSop(allowedSteps)(makeSop(_r))
        }
        val (res, r4) = RNG.double(builtSops.last._2)

        val sops = sop :: builtSops.map(_._1).toList

        val sopResult = res match {
          case x if x < 0.3 => Parallel(sops)
          case x if x < 0.6 => Sequence(sops)
          case _ => SOP(sops)
        }

        (sopResult, r4)
      }
    }
  }

  def buildSop(allowedSteps: Int)(s: (SOP, RNG)): (SOP, RNG) = {
    if (allowedSteps <= 0 || s._1.size > 45) s
    else {
      val (sop, rng) = s
      val (rand, r2) = RNG.double(rng)

      rand match {
        case x if x < 0.3 =>
          val newState = wrapSop((sop, r2))
          buildSop(allowedSteps - 1)(newState)
        case x if x <= 0.8 =>
          val newState = childSop(allowedSteps - 1, RNG.range(0, 4))((sop, r2))
          buildSop(allowedSteps - 1)(newState)
        case _ => s
      }
    }
  }

  def makeSop(rng: RNG): (SOP, RNG) = {
    val (operation, r2) = SopGeneration.operation(rng)

    (SOP(operation), r2)
  }
}