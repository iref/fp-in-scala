package fpinscala.ch03

import fpinscala.AbstractSpec

class Chapter3Spec extends AbstractSpec {

  "sum2" should "add all ints" in {
    val ints = List(1, 2, 3, 4)
    Chapter3.sum2(ints) should be(10)
  }

  it should "return 0 if list is empty" in {
    Chapter3.sum2(Nil) should be(0)
  }

  "product2" should "multiply all doubles in list" in {
    val doubles = List(1.0, -2.0, 3.0, -4.0)
    Chapter3.product2(doubles) should be(24.0)
  }

  it should "return 1.0 if list is empty" in {
    Chapter3.product2(Nil) should be (1.0)
  }

  "sum3" should "add all ints in list" in {
    val ints = List(1, 2, 3, 4)
    Chapter3.sum3(ints) should be (10)
  }

  it should "return 0 if list is empty" in {
    Chapter3.sum3(Nil) should be (0)
  }

  "product3" should "multiply all doubles in list" in {
    val ints = List(1.0, -2.0, 3.0, 4.0)
    Chapter3.product3(ints) should be (-24.0)
  }

  it should "return 1.0 if list is empty" in {
    Chapter3.product3(Nil) should be (1.0)
  }

  "foldLeftWithFoldRight" should "return same result as foldLeft" in {
    val ints = List(1, 2, 3, 4)
    val expected = ints.foldLeft(0)(_ - _)

    val actual = Chapter3.foldLeftWithFoldRight(ints, 0)(_ - _)

    actual should be(expected)
  }

  "foldRightWithFoldLeft" should "return same result as foldRight" in {
    val ints = List(1, 2, 3, 4)
    val expected = ints.foldRight(0)(_ - _)

    val actual = Chapter3.foldRightWithFoldLeft(ints, 0)(_ - _)

    actual should be(expected)
  }

  "increment" should "add 1 to each element of list" in {
    val ints = List(1, 2, 3, 4)
    
    Chapter3.increment(ints) should be(List(2, 3, 4, 5))
  }

  "doublesToString" should "convert each double into string" in {
    val doubles = List(1.0, 2.0, 3.0)

    Chapter3.doublesToString(doubles) should be(List("1.0", "2.0", "3.0"))
  }

  "add" should "add elements on same positions in list" in {
    val as = List(1, 2, 3, 4)
    val bs = List(5, 6, 7, 8)

    Chapter3.add(as, bs) should be (List(6, 8, 10, 12))
  }

  it should "omit longer suffix" in {
    val as = List(1, 2, 3, 4)
    val bs = List(5, 6, 7)

    val expected = List(6, 8, 10)

    Chapter3.add(as, bs) should be(expected)
    Chapter3.add(bs, as) should be(expected)
  }

  it should "return empty list if both list are empty" in {
    Chapter3.add(Nil, Nil) should be(Nil)
  }
}
