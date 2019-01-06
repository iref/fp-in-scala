package fpinscala.ch03

object Chapter3 {
  // Exercise 3.1
  //
  // First pattern does not match, because it requires 2 to be followed by 4
  // Seconds pattern does not match, because list is not Nil
  // Third patter matches, because first two elements are mapped to variables x and y
  // and next two elements match 3 and 4, so result is 1 + 2 = 3
  // Fourth an Fifth pattern are not checked
  
  def sum2(xs: List[Int]): Int =
    xs.foldRight(0)((x, acc) => x + acc)

  def product2(xs: List[Double]): Double =
    xs.foldRight(1.0)((x, acc) => x * acc)

  // Exercise 3.7
  // We don't have abstract way of defining when to stop while folding.
  // We can extend foldRight to take additional function (shouldStop) or 
  // return different type from reducer function to indicate if we should
  // continue to traverse list or we should stop
  // def foldRight[A, B](xs: List[A], z: B)(f: (A, B) => B, stop: B => Boolean): B
  
  // Exercise 3.8
  // Fold right reconstructs original list
  // We can say that Nil is 'zero' value and 'Cons' is reducers function
  // and foldRight is just abstract way of building algebraic data types
  // or other data structure

  // Exercise 3.11
  def sum3(xs: List[Int]): Int =
    xs.foldLeft(0)(_ + _)
  def product3(xs: List[Double]): Double =
    xs.foldLeft(1.0)(_ * _)

  // Exercise 3.13
  def foldLeftWithFoldRight[A, B](xs: List[A], acc: B)(f: (B, A) => B): B =
    xs.foldRight(acc) { (x, acc) =>
      f(acc, x)
    }

  def foldRightWithFoldLeft[A, B](xs: List[A], acc: B)(f: (A, B) => B): B =
    xs.reverse.foldLeft(acc) { (acc, x) =>
      f(x, acc)
    }
  
  // Exercise 3.16
  def increment(ints: List[Int]): List[Int] =
    ints.foldRight(Nil: List[Int])((i, acc) => Cons(i + 1, acc))

  // Exercise 3.17
  def doublesToString(doubles: List[Double]): List[String] =
    doubles.foldRight(Nil: List[String])((d, acc) => Cons(d.toString, acc))

  // Exercise 3.19
  def even(ints: List[Int]): List[Int] =
    ints.filter(i => i % 2 == 0)

  // Exercise 3.21
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as.flatMap { a =>
      if (f(a)) {
        List(a)
      } else {
        Nil
      }
    }

  // Exercise 3.22
  def add(as: List[Int], bs: List[Int]): List[Int] =
    (as, bs) match {
      case (Cons(a, ass), Cons(b, bss)) => Cons(a + b, add(ass, bss))
      case _ => Nil
    }

  // Exercise 3.24
  def take[A](list: List[A], n: Int): List[A] =
  // Note: We can use foldHere, but we loose short-circuiting
    list match {
      case Nil => Nil
      case Cons(_, _) if n <= 0 => Nil
      case Cons(h, t) => Cons(h, take(t, n - 1))
    }

  def subsequenciesOfN[A](l: List[A], n: Int): List[List[A]] =
    l match {
      case Nil => Nil
      case Cons(_, _) if l.length < n => Nil
      case Cons(_, t) => Cons(take(l, n), subsequenciesOfN(t, n))
    }

  def exists[A](list: List[A])(f: A => Boolean): Boolean =
  // NOTE: We can use foldHere, but we loose short-circuiting
    list match {
      case Nil => false
      case Cons(h, t) if f(h) => true
      case Cons(_, t) => exists(t)(f)
    }


  def hasSubsequence[A](sequence: List[A], subsequence: List[A]): Boolean = {
    val subsequencies = subsequenciesOfN(sequence, subsequence.length)
    exists(subsequencies)(_ == subsequence)
  }
}
