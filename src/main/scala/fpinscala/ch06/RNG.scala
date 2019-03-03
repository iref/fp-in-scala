package fpinscala.ch06

import scala.math.abs

trait RNG {
  def nextInt: (Int, RNG)
}

final case class SimpleRNG(val seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  // Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    val nonNegativeI = if (i == Int.MinValue) Int.MaxValue else math.abs(i)
    (nonNegativeI, rng2)
  }

  // Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = RNG.nonNegativeInt(rng)
    val d = i.toDouble / Int.MaxValue
    (d, rng2)
  }
  
  // Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = RNG.double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng3) = RNG.intDouble(rng)
    ((d, i), rng3)
  }

  def double3(rng:RNG): ((Double, Double, Double), RNG) = {
    val (d, rng2) = RNG.double(rng)
    val (d2, rng3) = RNG.double(rng2)
    val (d3, rng4) = RNG.double(rng3)
    ((d, d2, d3), rng4)
  }

  // Exercise 6.4
  def ints(n: Int)(rng: RNG): (List[Int], RNG) = {
    def go(n: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (n <= 0) {
        (acc, rng)
      } else {
        val (i, rng2) = rng.nextInt
        go(n - 1, rng2, i :: acc)
      }
    }

    go(n, rng, Nil)
  }
}
