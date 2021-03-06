package fpinscala.ch04

sealed trait Either[+E, +A] {
  // Exercise 4.6
  def map[EE >: E, B](f: A => B): Either[EE, B] =
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

  def orElse[EE >: E, AA >: A](default: => Either[EE, AA]): Either[EE, AA] =
    this match {
      case Right(a) => this
      case Left(e) => default
    }

  def map2[EE >: E, B, C](be: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap { a =>
      be.map { b =>
        f(a, b)
      }
    }
}

object Either {
  // Exercise 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight(Right(Nil): Either[E, List[A]]) { (e, acc) =>
      e.flatMap(a => acc.map(as => a :: as))
    }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(Nil): Either[E, List[B]]) { (a, acc) =>
      f(a).flatMap(b => acc.map(bs => b :: bs))
    }

  // Exercise 4.8
  //
  // We can introduce new data type called Validation
  // that captures multiple error into list Either[List[E], A]
  // and also has map2 function and either can be transform into it.
  // So in mkPerson we run validations that return either, than convert them
  // into validation datatype and use map2 over it.
}
case class Left[E](value: E) extends Either[E, Nothing]
case class Right[A](value: A) extends Either[Nothing, A]
