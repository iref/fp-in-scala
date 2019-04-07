package fpinscala.ch05

import fpinscala.AbstractSpec

class StreamSpec extends AbstractSpec {

  "toList" should "force evaluation and store results in list" in {
    val s = Stream.cons(1 + 1, Stream.cons(1 + 2, Stream.cons(1 + 3, Stream.empty)))
    s.toList should be(List(2, 3, 4))
  }

  "take" should "return stream with first n elements" in {
    val s = Stream(1, 2, 3, 4, 5).take(3)
    s.toList should be(List(1, 2, 3))
  }

  it should "return whole stream if it has less elements than n" in {
    val s = Stream(1, 2, 3, 4).take(5)
    s.toList should be(List(1, 2, 3, 4))
  }

  "drop" should "return stream without first n elements" in {
    val s = Stream(1, 2, 3, 4, 5).drop(2)
    s.toList should be(List(3, 4, 5))
  }

  it should "return empty stream if it has less elements than n" in {
    val s = Stream(1, 2, 3, 4, 5).drop(6)
    s.toList should be(Nil)
  }

  it should "return empty stream if stream is empty" in {
    val s = Stream.empty[Int].drop(2)
    s.toList should be(Nil)
  }

  "takeWhile" should "return all elements until first rejected elements" in {
    val s = Stream(1, 2, 3, 4, 5).takeWhile(a => a < 4)
    s.toList should be(List(1, 2, 3))
  }

  it should "return whole stream if predicate is never false" in {
    val s = Stream(1, 2, 3, 4, 5).takeWhile(a => a < 10)
    s.toList should be(List(1, 2, 3, 4, 5))
  }
}
