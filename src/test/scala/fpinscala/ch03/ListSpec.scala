package fpinscala.ch03

import fpinscala.AbstractSpec

class ListSpec extends AbstractSpec {

  "tail" should "remove first element from the list" in {
    val xs = List(1, 2, 3, 4, 5)
    xs.tail should be(List(2, 3, 4, 5))
  }

  it should "throw IllegalStateException if list is empty" in {
    val xs: List[Int] = Nil
    an [IllegalStateException] should be thrownBy(xs.tail)
  }

  "setHead" should "replace head of non-empty list" in {
    val xs = List(1, 2, 3, 4, 5)
    xs.setHead(10) should be(List(10, 2, 3, 4, 5))
  }

  it should "add new head to empty list" in {
    val xs: List[Int] = Nil
    xs.setHead(1) should be(List(1))
  }

  "drop" should "return list without first n elements" in {
    val xs = List(1, 2, 3, 4, 5)
    xs.drop(2) should be(List(3, 4, 5))
  }

  it should "return empty list if n is greater than list size" in {
    val xs = List(1, 2, 3, 4, 5)
    xs.drop(6) should be(Nil)
  }

  it should "return empty list if list is empty" in {
    val xs: List[Int] = Nil
    xs.drop(1) should be(Nil)
  }

  it should "return empty list if n is equal to list size" in {
    val xs = List(1, 2, 3, 4, 5)
    xs.drop(5) should be(Nil)
  }

  "dropWhile" should "return list without longest prefix accepting predicate" in {
    val xs = List(1, 1, 1, 3, 4, 6, 7)
    val actual = xs.dropWhile(x => x % 2 == 1)
    actual should be(List(4, 6, 7))
  }

  it should "return original list if all elements are rejected by predicate" in {
    val xs = List(1, 1, 1, 3, 3, 5)
    val actual = xs.dropWhile(x => x % 2 == 0)
    actual should be(List(1, 1, 1, 3, 3, 5))
  }

  it should "return empty list if all elements are accepted by predicate" in {
    val xs = List(2, 2, 2, 4, 4, 6)
    val actual = xs.dropWhile(x => x % 2 == 0)
    actual should be(Nil)
  }

  "init" should "remove last element of list" in {
    val xs = List(1, 2, 3, 4, 5)
    xs.init should be(List(1, 2, 3, 4))
  }

  it should "return empty list as init of empty list" in {
    val xs = List[Int]()
    xs.init should be(Nil)
  }

  "length" should "return correct length of non-empty list" in {
    val xs = List(1, 2, 3, 4)
    xs.length should be(4)
  }

  it should "return lenght zero if list is empty" in {
    Nil.length should be(0)
  }

  "foldLeft" should "apply accumulator from left to right" in {
    val ints = List(1, 2, 3, 4)

    val actual = ints.foldLeft(0)(_ - _)

    actual should be(-10)
  }

  "reverse" should "return reversed list" in {
    val ints = List(1, 2, 3, 4)

    ints.reverse should be(List(4, 3, 2, 1))
  }

  it should "return empty list if original list is empty" in {
    Nil.reverse should be(Nil)
  }
}
