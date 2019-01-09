package fpinscala.ch02

object Chapter2 {
  
  def fib(n: Int): Int = {
    def go(prev: Int, current: Int, acc: Int): Int = {
      if (acc == 0) {
        prev
      } else {
        go(current, prev + current, acc - 1)
      }
    }

    // This can be handled without exception using functional error handling types
    // see Chapter 4
    if (n <= 0) {
      throw new IllegalArgumentException("n must be nonnegative integer")
    }

    go(0, 1, n - 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def go(as: Array[A], previous: A): Boolean = {
      if (as.isEmpty) {
        true
      } else if (!ordered(previous, as.head)) {
        false
      } else {
        go(as.tail, as.head)
      }
    }

    if (as.isEmpty) {
      true
    } else {
      go(as.tail, as.head)
    }
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](g: B => C, f: A => B): A => C =
    a => g(f(a))
}
