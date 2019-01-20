package fpinscala.ch04

import fpinscala.AbstractSpec

class OptionSpec extends AbstractSpec {

  "map" should "apply function if value is present" in {
    Some(1).map(a => a + 2) should be(Some(3))
  }

  it should "return None if value is missing" in {
    (None: Option[Int]).map(a => a + 2) should be(None)
  }

  "flatMap" should "apply function if value is present" in {
    Some(1).flatMap(a => Some(a + 2)) should be(Some(3))
  }

  it should "return None if function returns None" in {
    Some(1).flatMap(_ => None) should be(None)
  }

  it should "return None if original value is None" in {
    (None: Option[Int]).flatMap(a => Some(a + 2)) should be(None)
  }

  "getOrElse" should "return original value if it is present" in {
    Some(1).getOrElse(2) should be(1)
  }

  it should "return default value if value is missing" in {
    None.getOrElse(1) should be(1)
  }

  "orElse" should "return original value if it is present" in {
    Some(1).orElse(Some(2)) should be(Some(1))
  }

  it should "return default value if value is missing" in {
    None.orElse(Some(1)) should be(Some(1))
  }

  "filter" should "return original value if it's accepted by predicate" in {
    Some(1).filter(a => a % 2 == 1) should be(Some(1))
  }

  it should "return None if value is rejected by predicate" in {
    Some(1).filter(a => a % 2 == 0) should be(None)
  }

  it should "return None if value is missing" in {
    (None: Option[Int]).filter(a => a % 2 == 0) should be(None)
  }

  "map2" should "apply function if both values are present" in {
    val a = Some(1)
    val b = Some(2)

    Option.map2(a, b)((a, b) => a + b) should be(Some(3))
  }

  it should "return None if either values is None" in {
    val none: Option[Int] = None
    val some = Some(1)

    Option.map2(some, none)((a, b) => a + b) should be(None)
    Option.map2(none, some)((a, b) => a + b) should be(None)
    Option.map2(none, none)((a, b) => a + b) should be(None)
  }

  "sequence" should "return list of values if all are present" in {
    val l: List[Option[Int]] = List(Some(1), Some(2), Some(3), Some(4))
    Option.sequence(l) should be(Some(List(1, 2, 3, 4)))
  }

  it should "return None if at least one value is missing" in {
    val l = List(Some(1), None, Some(3), Some(4))
    Option.sequence(l) should be(None)
  }

  "traverse" should "return list of values if all are present" in {
    val l = List("a", "ab", "abc", "abcd")
    val result = Option.traverse(l) { a =>
      Some(a).filter(a => a.length > 0).map(_.length)
    }
    
    result should be(Some(List(1, 2, 3, 4)))
  }

  it should "return None if at least one value is missing" in {
    val l = List(1, 2, 3, 4)
    val result = Option.traverse(l) { a =>
      Some(a).filter(a => a % 2 == 0)
    }
    
    result should be(None)
  }

}
