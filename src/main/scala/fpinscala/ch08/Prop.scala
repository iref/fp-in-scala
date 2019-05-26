package fpinscala.ch08

import fpinscala.ch05._
import fpinscala.ch06._

case class Prop(run: (Prop.MaxSize, Prop.TestCases, RNG) => Result) { self =>
    
    // Exercise 8.9
    def &&(other: Prop): Prop =
        Prop { case (max, tc, rng) =>
            self.run(max, tc, rng) match {                
                case Falsified(fc, sc) => Falsified(fc, sc)
                case _ => other.run(max, tc, rng)
            }
        }

    def ||(other: Prop): Prop =
        Prop { case (max, tc, rng) =>
            self.run(max, tc, rng) match {
                case Falsified(fc, sc) => other.run(max, tc, rng)
                case x => x
            }
        }   
}

object Prop {
    type MaxSize = Int
    type SuccessCount = Int
    type FailedCase = String    
    type TestCases = Int

    def run(
        prop: Prop,
        maxSize: Int = 100,
        testCases: Int = 100,
        rng: RNG = SimpleRNG(System.currentTimeMillis())
    ): Unit =
        prop.run(maxSize, testCases, rng) match {
            case Passed =>
                println(s"+ OK, passed $testCases.")
            case Proved =>
                println(s"+ OK, proved property.")
            case Falsified(failedCase, successCount) =>
                println(s"! Falsified after $successCount passed test:\n $failedCase")
        }

    def forAll[A](as: Gen[A])(f: A => Boolean) = Prop {
        (n, tc, rng) => 
            randomStream(as)(rng)
                .zip(Stream.from(0))
                .take(n)
                .map { case (a, i) =>
                     runTest(i.toString, a, i)(f) 
                }
                .find(_.isFalsified)
                .getOrElse(Passed)
    }

    def forAll[A](as: SGen[A])(f: A => Boolean): Prop =
        forAll(size => as(size))(f)

    private def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop =
        Prop {
            case (max, tc, rng) =>
                val casesPerSuite = (tc + (max - 1)) / max
                val props: Stream[Prop] =
                    Stream.from(0)
                        .take(tc.min(max) + 1)
                        .map(i => forAll(g(i))(f))
                val prop: Prop =
                    props
                        .map { p => 
                            Prop { (max, _, rng) =>
                                p.run(max, casesPerSuite, rng)
                            }
                        }
                        .toList
                        .reduce(_ && _)
                prop.run(max, tc, rng)
                    
        }

    def label(label: String)(prop: Prop): Prop =
        Prop { case (maxSize, tc, rng) =>
            prop.run(maxSize, tc, rng) match {
                case Falsified(tc, sc) => Falsified(s"$label\n$tc", sc)
                case r => r
            }
        }

    private def randomStream[A](gen: Gen[A])(rng: RNG): Stream[A] =
        Stream.unfold(rng)(rng => Some(gen.sample.run(rng)))

    private def buildMsg[A](label: String, a: A, e: Exception): String =        
        s"test case: $a\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    private def runTest[A](label: String, a: A, i: Int)(f: A => Boolean): Result =
        try {
            if (f(a)) Passed else Falsified(s"$label: ${a.toString}", i)
        } catch {
            case e: Exception => Falsified(buildMsg(label, a, e), i)
        }
}

sealed trait Result {
    def isFalsified: Boolean
}

case object Passed extends Result {
    override def isFalsified = false
}

case object Proved extends Result {
    override def isFalsified = false
}

case class Falsified(failure: Prop.FailedCase, successes: Prop.SuccessCount) extends Result {
    override def isFalsified = true
}

case class Gen[A](sample: State[RNG, A]) {

    def map[B](f: A => B): Gen[B] =
        Gen(this.sample.map(f))

    // Exercise 8.6
    def flatMap[B](f: A => Gen[B]): Gen[B] =
        Gen(sample.flatMap(a => f(a).sample))
    
    def listOf(size: Gen[Int]): Gen[List[A]] =
        size.flatMap(n => Gen.listOfN(n, this))

    // Exercise 8.10
    def unsized: SGen[A] =
        SGen(_ => this)
}

object Gen {
    def nonNegativeInt: Gen[Int] =
        Gen(State(RNG.nonNegativeInt))

    def double: Gen[Double] =
        Gen(State(RNG.double))

    // Exercise 8.4
    def choose(start: Int, endInclusive: Int): Gen[Int] =
        Gen(State(RNG.nonNegativeInt).map(n => start + n % (endInclusive - start)))

    // Exercise 8.5
    def unit[A](a: => A): Gen[A] =
        Gen(State.unit(a))

    def boolean: Gen[Boolean] =
        Gen(State(RNG.nonNegativeInt).map(n => n % 2 == 0))

    def listOfN[A](n: Int, gen: Gen[A]): Gen[List[A]] =
        Gen(State.sequence(List.fill(n)(gen.sample)))    

    // Exercise 8.7
    def union[A](gen1: Gen[A], gen2: Gen[A]): Gen[A] =
        Gen.boolean.flatMap { b =>
            if (b) gen1 else gen2
        }    
}

case class SGen[A](forSize: Int => Gen[A]) {
    def apply(size: Int): Gen[A] = forSize(size)
    // Exercise 8.11
    def map[B](f: A => B): SGen[B] =
        SGen(size => forSize(size).map(f))

    def flatMap[B](f: A => SGen[B]): SGen[B] =
        SGen(size => forSize(size).flatMap(a => f(a).forSize(size)))    
}

object SGen {

    // Exercise 8.12
    def listOf[A](gen: Gen[A]): SGen[List[A]] =
        SGen(size => Gen.listOfN(size, gen))

    // Exercise 8.13
    def listOf1[A](g: Gen[A]): SGen[List[A]] =
        SGen(n => Gen.listOfN(n max 1, g))
}