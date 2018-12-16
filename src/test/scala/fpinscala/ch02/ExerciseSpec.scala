package fpinscala.ch02

import fpinscala.AbstractSpec

class ExerciseSpec extends AbstractSpec {

  "fib" should "correctly compute nth fibonacci number" in {
    Chapter2.fib(5) should be(5)
    Chapter2.fib(0) should be(0)
    Chapter2.fib(1) should be(1)
    Chapter2.fib(10) should be(55)
  }

  it should "throw exception if n is negative integer" in {
    an [IllegalArgumentException] should be thrownBy Chapter2.fib(-1)
  }

  "isSorted" should "return true if array is sorted based on comparator" in {
    val ordered = (l: Int, r: Int) => l < r
    Chapter2.isSorted(Array(1, 2, 3, 4, 9, 10, 1000), ordered) should be(true)
  }

  it should "return false if array is not sorted based on comparator" in {
    val ordered = (l: Int, r: Int) => l < r
    Chapter2.isSorted(Array(1, 2, 1999, 4, 5, 1000, 500), ordered) should be(false)
  }

  it should "return true if array is empty" in {
    val ordered = (l: Int, r: Int) => true
    Chapter2.isSorted(Array[Int](), ordered) should be(true)
  }

  "curry" should "return same result as original function" in {
    val curried = Chapter2.curry[Int, Int, Int](_ + _)
    curried(1)(2) should be(1 + 2)
  }

  "uncurry" should "return same result as original function" in {
    val f: Int => Int => Int = a => b => a + b
    val uncurried = Chapter2.uncurry[Int, Int, Int](f)
    uncurried(1, 2) should be(f(1)(2))
  }

  "compose" should "correctly chain two functions" in {
    val add1 = (a: Int) => a + 1
    val printResult = (a: Int) => s"Result: $a"

    val composed = Chapter2.compose(printResult, add1)
    composed(10) should be("Result: 11")
  }
}
