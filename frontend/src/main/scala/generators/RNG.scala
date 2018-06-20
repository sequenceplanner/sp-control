package generators

trait RNG {
  def nextInt: (Int, RNG)
}

/**
  * Originally from https://github.com/fpinscala/fpinscala
  * 6/19/2018
  */
object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    val res = if (i < 0) -(i + 1) else i

    (res, r)
  }

  def int: Rand[Int] = _.nextInt
  def ints(n: Int): Rand[List[Int]] = sequence(List.fill(n)(int))


  def range(startInclusive: Int, endExclusive: Int): Rand[Int] = rng => {
    val (i, r) = nonNegativeInt(rng)
    val percent = i / Integer.MAX_VALUE.toDouble

    val res = startInclusive + (endExclusive - startInclusive) * percent
    (res.round.toInt, r)
  }

  def sequence[A](xs: List[Rand[A]]): Rand[List[A]] = xs match {
    case Nil => map(unit(Unit))(_ => Nil)
    case h :: t => map2(h, sequence(t))(_ :: _)
  }

  def boolean(rng: RNG): (Boolean, RNG) = {
    val (i, r) = rng.nextInt

    (i % 2 == 0, r)
  }

  type Rand[+A] = RNG => (A, RNG)

  val double: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r1) = f(rng)
    g(a)(r1)
  }

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))

}
