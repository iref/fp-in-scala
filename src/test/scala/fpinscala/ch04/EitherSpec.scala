package fpinscala.ch04

import fpinscala.AbstractSpec

class EitherSpec extends AbstractSpec {

  "map" should "apply function if value is right" in {
    val e: Either[String, Int] = Right(2)

    e.map(a => "a" * a) should be(Right("aa"))
  }

  it should "keep error if value is left" in {
    val e: Either[String, Int] = Left("Terrible error")

    e.map(a => a) should be(e)
  }

  "flatMap" should "apply function if value is right" in {
    val e: Either[String, Int] = Right(2)

    e.flatMap(a => Right("b" * a)) should be(Right("bb"))
  }

  it should "return left if function return left" in {
    val e: Either[String, Int] = Right(2)

    e.flatMap(_ => Left("Terrible flatMap error")) should be(Left("Terrible flatMap error"))
  }

  it should "return left if original either is left" in {
    val e: Either[String, Int] = Left("Terrible error")

    e.flatMap(a => Right(a)) should be(e)
  }

  "orElse" should "return original value if either is right" in {
    val e: Either[String, Int] = Right(2)

    e.orElse(Right(3)) should be(Right(2))
  }

  it should "return default value if either is left" in {
    val e: Either[String, Int] = Left("terrible error")

    e.orElse(Right(3)) should be(Right(3))
  }

  "map2" should "apply function if both eithers are right" in {
    val e1: Either[String, Int] = Right(2)
    val e2: Either[String, Int] = Right(3)

    e1.map2(e2)((a, b) => a + b) should be(Right(5))
  }

  it should "return left error if either of eithers is left" in {
    val left: Either[String, Int] = Left("Terrible error")
    val left2: Either[String, Int] = Left("Terrible error 2")
    val right: Either[String, Int] = Right(2)

    left.map2(right)((a, b) => a + b) should be(left)
    right.map2(left)((a, b) => a + b) should be(left)
    left.map2(left2)((a, b) => a + b) should be(left)
  }

  "sequence" should "return right if all values are right" in {
    val es: List[Either[String, Int]] = List(Right(1), Right(2), Right(3))
    Either.sequence(es) should be(Right(List(1, 2, 3)))
  }

  it should "return left if any value is left" in {
    val es: List[Either[String, Int]] = List(Right(1), Left("Terrible error"), Right(3))
    Either.sequence(es) should be(Left("Terrible error"))
  }

  "traverse" should "return right of list of results if all mapped values return right" in {
    val es = List(1, 2, 3, 4)
    val result = Either.traverse(es)(a => Right(a))
    result should be(Right(List(1, 2, 3, 4)))
  }

  it should "return left if any mapped value is left" in {
    val es = List(1, 2, 3, 4)
    val result = Either.traverse(es) { a =>
      if (a % 2 == 0)
        Right(a)
      else
        Left(s"$a is odd number")
    }

    result should be(Left("1 is odd number"))
  }
}
