package fpinscala.ch06

object Random {
  type Random[+A] = RNG => (A, RNG)

  def unit[A](a: A): Random[A] =
    rng => (a, rng)

  def map[A, B](random: Random[A])(f: A => B): Random[B] =
    rng => {
      val (a, rng2) = random(rng)
      (f(a), rng2)
    }

  def nonNegativeInt: Random[Int] =
    rng => RNG.nonNegativeInt(rng)

  def nonNegativeEven: Random[Int] =
    map(RNG.nonNegativeInt)(a => a - a % 2)

  // Exercise 6.5
  def double: Random[Double] =
    map(RNG.nonNegativeInt) { i =>
      i.toDouble / Int.MaxValue
    }

  def int: Random[Int] =
    rng => rng.nextInt

  // Exercise 6.6
  def map2[A, B, C](ra: Random[A], rb: Random[B])(f: (A, B) => C): Random[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def both[A, B](ra: Random[A], rb: Random[B]): Random[(A, B)] =
    map2(ra, rb)((a, b) => (a, b))

  def randIntDouble: Random[(Int, Double)] =
    both(int, double)

  def randDoubleInt: Random[(Double, Int)] =
    both(double, int)

  // Exercise 6.7
  def sequence[A](ras: List[Random[A]]): Random[List[A]] =
    ras.foldLeft(unit(Nil): Random[List[A]]) { (acc, ra) =>
      map2(ra, acc)(_ :: _)
    }

  def ints(n: Int): Random[List[Int]] =
    sequence(List.fill(n)(Random.int))

  // Exercise 6.8
  def flatMap[A, B](ra: Random[A])(f: A => Random[B]): Random[B] =
    rng => {
      val (a, rng2) = ra(rng)
      f(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Random[Int] =
    flatMap(Random.nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) {
        Random.unit(mod)
      } else {
        Random.nonNegativeLessThan(n)
      }
    }

  // Exercise 6.9
  def map_flatMap[A, B](ra: Random[A])(f: A => B): Random[B] =
    flatMap(ra) { a => unit(f(a)) }

  def map2_flatMap[A, B, C](ra: Random[A], rb: Random[B])(f: (A, B) => C): Random[C] =
    flatMap(ra) { a =>
      map(rb) { b =>
        f(a, b)
      }
    }

  def rollDie: Random[Int] = map(nonNegativeLessThan(6))(r => r + 1)
}
