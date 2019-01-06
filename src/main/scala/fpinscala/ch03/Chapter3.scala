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
  def foldLeftWithFoldRight[A, B](xs: List[A], acc: B)(f: (B, A) => B): B = {
    xs.foldRight(acc) { (x, acc) =>
      f(acc, x)
    }
  }
}
