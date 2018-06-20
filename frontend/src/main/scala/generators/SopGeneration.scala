package generators

import sp.domain._
import RNG.Rand

/**
  * Utility for generating a SOP for testing purposes
  */
object SopGeneration {
  val operation: Rand[Operation] = RNG.double andThen { case (d, r) => (Operation(d.toString), r) }

  // Sequence of operations generator
  def sop(explosionFactor: Int = 8): Rand[SOP] = rng => SOPHelpers.buildSop(explosionFactor)(EmptySOP, rng)

  val sop: Rand[SOP] = sop()

  def showSop(sop: SOP): String = {
    def indent(depth: Int): String = List.fill(depth)("  ").mkString

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
      val rest = sop.sop.map(s => show(s, depth + 1)).mkString
      r1 + rest
    }

    show(sop, 0)
  }
}

// TODO Add more SOP classes
// TODO Clean up magic numbers (mostly % chances)
private object SOPHelpers {
  implicit class SopWithSize(sop: SOP) {
    def size: Int = {
      if (sop.sop.isEmpty) 1
      else 1 + sop.sop.map(s => s.size).sum
    }
  }

  def wrapSop(s: (SOP, RNG)): (SOP, RNG) = {
    val (sop, r1) = s
    val (res, r2) = RNG.double(r1)

    if (sop == EmptySOP) (EmptySOP, r2)
    else {
      val list = List(sop)

      val newSop = res match {
        case x if x < 1/3 => Parallel(list)
        case x if x < 2/3 && x >= 1/3 => Sequence(list)
        case x if x <= 1 && x >= 2/3 => SOP(list)
      }

      (newSop, r2)
    }
  }

  def childSop(allowedSteps: Int, childCountRange: Rand[Int])(data: (SOP, RNG)): (SOP, RNG) = {
    if (allowedSteps <= 0) data
    else {
      val (sop, r1) = data

      if (sop == EmptySOP) makeSop(r1)
      else {

        val (childCount, r2) = childCountRange(r1)
        val firstChild = buildSop(allowedSteps)(makeSop(r2))
        val builtSops = (0 to childCount).scanLeft(firstChild) { case ((_, rng), _) =>
          buildSop(allowedSteps)(makeSop(rng))
        }
        val (_, r4) = builtSops.last
        val (res, r5) = RNG.double(r4)

        val sops = sop :: builtSops.map { case (newSop, _) => newSop }.toList

        val sopResult = res match {
          case x if x < 0.3 => Parallel(sops)
          case x if x < 0.6 => Sequence(sops)
          case _ => SOP(sops)
        }

        (sopResult, r5)
      }
    }
  }

  def buildSop(allowedSteps: Int)(s: (SOP, RNG)): (SOP, RNG) = {
    val (sop, rng) = s

    if (allowedSteps <= 0 || sop.size > 45) s
    else {
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