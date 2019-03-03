package fpinscala.ch06

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import fpinscala.AbstractSpec

class RNGSpec extends AbstractSpec {

  val seedAndInt: Gen[(Long, Int)] =
    for {
      seed <- arbitrary[Long]
      n <- Gen.choose(0, 50)
    } yield (seed, n)

  "nonNegativeInt" should "always return positive integer" in {
    forAll { seed: Long =>
      val rng = SimpleRNG(seed)
      val (i, _) = RNG.nonNegativeInt(rng)
      i should be >= 0
    }
  }

  "double" should "always return double between 0 and 1" in {
    forAll { seed: Long =>
      val rng = SimpleRNG(seed)
      val (d, _) = RNG.double(rng)
      d should be >= 0.0
      d should be < 1.0
    }
  }

  "intDouble" should "always return valid (int, double)" in {
    forAll { seed: Long =>
      val rng = SimpleRNG(seed)
      val ((i, d), _) = RNG.intDouble(rng)
      i should be <= Int.MaxValue
      i should be >= Int.MinValue
      d should be >= 0.0
      d should be < 1.0
    }
  }

  "doubleInt" should "always return valid (double, int)" in {
    forAll { seed: Long =>
      val rng = SimpleRNG(seed)
      val ((d, i), _) = RNG.doubleInt(rng)
      i should be <= Int.MaxValue
      i should be >= Int.MinValue
      d should be >= 0.0
      d should be < 1.0
    }
  }

  "double3" should "always return valid triple" in {
    forAll { seed: Long =>
      val rng = SimpleRNG(seed)
      val ((d1, d2, d3), _) = RNG.double3(rng)

      d1 should be >= 0.0
      d1 should be < 1.0
      d2 should be >= 0.0
      d2 should be < 1.0
      d3 should be >= 0.0
      d3 should be < 1.0
    }
  }

  "ints" should "generate list of integers" in {
    forAll(seedAndInt) { input: (Long, Int) =>
      val (seed, n) = input
      val rng = SimpleRNG(seed)
      val (is, _) = RNG.ints(n)(rng)

      is.length should be(n)
      is.foreach { i =>
        i should be <= Int.MaxValue
        i should be >= Int.MinValue
      }
    }
  }
}
