package fpinscala.ch06

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import fpinscala.AbstractSpec

class RandomSpec extends AbstractSpec {

  val seedAndInt: Gen[(Long, Int)] =
    for {
      seed <- arbitrary[Long]
      n <- Gen.choose(0, 50)
    } yield (seed, n)

  "nonNegativeEven" should "always return positive integer" in {
    forAll { seed: Long =>
      val rng = SimpleRNG(seed)
      val (i, _) = Random.nonNegativeEven(rng)
      i should be >= 0
      (i % 2) should be(0)
    }
  }

  "double" should "always return double between 0 and 1" in {
    forAll { seed: Long =>
      val rng = SimpleRNG(seed)
      val (d, _) = Random.double(rng)
      d should be >= 0.0
      d should be < 1.0
    }
  }

  "map" should "correctly transition one state value to new one" in {
    forAll { seed: Long =>
      val rng = SimpleRNG(seed)
      val oddInts = Random.map(RNG.nonNegativeInt)(a => if (a % 2 == 0) a + 1 else a)
      val (result, _) = oddInts(rng)
      result should be >= 0
      (result % 2) should be(1)
    }
  }

  "map2" should "correctly combine two random values" in {
    forAll { seed: Long =>
      val rng = SimpleRNG(seed)
      val sum = Random.map2(RNG.nonNegativeInt, RNG.nonNegativeInt)((a, b) => a + b)
      val (result, _) = sum(rng)

      result should be >= rng.nextInt._1
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
