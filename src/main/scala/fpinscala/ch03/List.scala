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
    this.foldLeft(Nil: List[A])((acc, x) => Cons(x, acc))
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
}
