package fpinscala.ch10

import fpinscala.AbstractSpec
import fpinscala.ch08._
import fpinscala.ch10.Monoid._

class MonoidSpec extends AbstractSpec {

    private def leftIdentity[A](monoid: Monoid[A])(gen: Gen[A]): Prop =
        Prop.forAll(gen) { a =>
            monoid.op(monoid.zero, a) == a
        }

    private def rightIdentity[A](monoid: Monoid[A])(gen: Gen[A]): Prop =
        Prop.forAll(gen) { a =>
            monoid.op(a, monoid.zero) == a
        }

    private def associativity[A](monoid: Monoid[A])(gen: Gen[A]): Prop =
        Prop.forAll(Gen.listOfN(3, gen)) { case x :: y :: z :: Nil =>
            monoid.op(monoid.op(x, y), z) == monoid.op(x, monoid.op(y, z))
        }

    // Exercise 10.4
    def monoidLaws[A](monoid: Monoid[A])(gen: Gen[A]): Unit =
        Prop.run(
            leftIdentity(monoid)(gen) && rightIdentity(monoid)(gen) && associativity(monoid)(gen)
        )

    "intAdditiveMonoid" should "comply with monoid laws" in {
        monoidLaws(intAdditionMonoid)(Gen.nonNegativeInt)
    }

    "intMultiplicationMonoid" should "comply with monoid laws" in {
        monoidLaws(intMultiplicationMonoid)(Gen.nonNegativeInt)
    }

    "booleanAndMonoid" should "comply with monoid laws" in {
        monoidLaws(booleanAndMonoid)(Gen.boolean)
    }

    "booleanOrMonoid" should "comply with monoid laws" in {
        monoidLaws(booleanOrMonoid)(Gen.boolean)
    }

    "wcMonoid" should "comply with monoid laws" in {
        val stubGen: Gen[WC] =
            Gen.string.map(Stub)
        val partGen: Gen[WC] =
            for {
                l <- Gen.string
                wc <- Gen.nonNegativeInt
                r <- Gen.string
            } yield Part(l, wc, r)
        val wcGen: Gen[WC] =
            Gen.double.flatMap { d =>
                if (d <= 0.6) stubGen else partGen
            }
        monoidLaws(wordCountMonoid)(wcGen)
    }

    "productMonoid" should "comply with monoid laws" in {
        val gen =
            for {
                i <- Gen.nonNegativeInt
                b <- Gen.boolean
            } yield (i, b)

        monoidLaws(productMonoid(intAdditionMonoid, booleanOrMonoid))(gen)
    }
}
