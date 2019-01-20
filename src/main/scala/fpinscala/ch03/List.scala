package fpinscala.ch03

sealed trait List[+A] {
  // Exercise 3.2
  // Can also use Try, Either if list is empty
  def tail: List[A] = this match {
    case Cons(_, xs) => xs
    case Nil => throw new IllegalStateException("Empty list doesn't have tail")
  }

  // Exercise 3.3
  def setHead[AA >: A](a: AA): List[AA] = this match {
    case Cons(_, xs) => Cons(a, xs)
    case Nil => Cons(a, Nil)
  }

  // Exercise 3.4
  def drop(n: Int): List[A] = this match {
    case Nil => Nil
    case Cons(_, xs) if n > 0 => xs.drop(n - 1)
    case xs => xs
  }

  // Exercise 3.5
  def dropWhile(predicate: A => Boolean): List[A] = this match {
    case Cons(x, xs) if predicate(x) => xs.dropWhile(predicate)
    case xs => xs
  }

  // Exercise 3.6
  // Maybe we should throw exception for empty list??
  def init[AA >: A]: List[AA] = this match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, xs.init)
  }

  def foldRight[B](acc: B)(f: (A, B) => B): B =
    this match {
      case Nil => acc
      case Cons(x, xs) => f(x, xs.foldRight(acc)(f))
    }

  // Exercise 3.9
  def length: Int = this.foldRight(0)((_, acc) => acc + 1)

  // Exercise 3.10
  // foldRight is not tail-recursive because first we have to find last
  // element of list and apply reducer function to it and zero value
  // and than we call reducer on result and element before last element,
  // therefor recursive call is not last call.
  def foldLeft[B](acc: B)(f: (B, A) => B): B = {
    def step(xs: List[A], acc: B): B = xs match {
      case Nil => acc
      case Cons(x, xs) => xs.foldLeft(f(acc, x))(f)
    }
    step(this, acc)
  }

  // Exercise 3.12
  def reverse: List[A] =
    foldLeft(Nil: List[A])((acc, x) => Cons(x, acc))

  // Exercise 3.14
  def append[AA >: A](value: AA): List[AA] =
    foldRight(Cons(value, Nil))(Cons.apply)

  // Exercise 3.18
  def map[B](f: A => B): List[B] =
    foldRight(Nil: List[B])((a, acc) => Cons(f(a), acc))

  // Exercise 3.19
  def filter(f: A => Boolean): List[A] =
    foldRight(Nil: List[A]) { (a, acc) =>
      if (f(a)) {
        Cons(a, acc)
      } else {
        acc
      }
    }

  // Exercise 3.20
  //
  // We can make this more efficient if we map and
  // concat elements in same foldRight call
  def flatMap[B](f: A => List[B]): List[B] =
    List.concat(map(f))

  def flatMap2[B](f: A => List[B]): List[B] =
    foldRight(Nil: List[B]) { (a, acc) =>
      val bs = f(a)
      bs.foldRight(acc)((b, acc) => Cons(b, acc))
    }

  def zipWith[B, C](bs: List[B])(f: (A, B) => C): List[C] =
    (this, bs) match {
      case (Cons(a, ass), Cons(b, bss)) => Cons(f(a, b), ass.zipWith(bss)(f))
      case _ => Nil
    }
}

final case object Nil extends List[Nothing]
final case class Cons[+A](x: A, xs: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) {
      Nil
    } else {
      Cons(as.head, apply(as.tail: _*))
    }

  def sum(as: List[Int]): Int =
    as match {
      case Nil => 0
      case Cons(h, t) => h + sum(t) // TODO can be tail recursive
    }

  def product(as: List[Double]): Double =
    as match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(h, t) => h * product(t) // TODO can be tail recursive
    }

  def drop[A](xs: List[A], n: Int): List[A] = xs.drop(n)
  def dropWhile[A](xs: List[A], predicate: A => Boolean): List[A] = xs.dropWhile(predicate)
  def init[A](xs: List[A]): List[A] = xs.init
  def foldRight[A, B](xs: List[A], acc: B)(f: (A, B) => B): B = xs.foldRight(acc)(f)
  def length[A](xs: List[A]): Int = xs.length
  def foldLeft[A, B](xs: List[A], acc: B)(f: (B, A) => B): B = xs.foldLeft(acc)(f)
  def reverse[A](xs: List[A]): List[A] = xs.reverse
  def append[A](xs: List[A], value: A): List[A] = xs.append(value)
  def map[A, B](xs: List[A])(f: A => B): List[B] = xs.map(f)
  def filter[A](xs: List[A])(f: A => Boolean): List[A] = xs.filter(f)
  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = xs.flatMap(f)

  // Exercise 3.15
  def concat[A](lists: List[List[A]]): List[A] =
    lists.foldRight(Nil: List[A]) { (as, acc) =>
      as.foldRight(acc)(Cons.apply)
    }

  // Exercise 3.23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    (as, bs) match {
      case (Cons(a, ass), Cons(b, bss)) => Cons(f(a, b), zipWith(ass, bss)(f))
      case _ => Nil
    }
}
