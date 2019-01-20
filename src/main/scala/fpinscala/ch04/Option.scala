package fpinscala.ch04

sealed trait Option[+A] {
  // Exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[AA >: A](default: => AA): AA = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[AA >: A](default: => Option[AA]): Option[AA] = this match {
    case None => default
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => if (f(a)) this else None
  }
}

object Option {
  // Exercise 4.3
  def map2[A, B, C](ao: Option[A], bo: Option[B])(f: (A, B) => C): Option[C] =
    (ao, bo) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None
    }

  def map2_2[A, B, C](ao: Option[A], bo: Option[B])(f: (A, B) => C): Option[C] =
    ao.flatMap { a =>
      bo.map(b => f(a, b))
    }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

  // Exercise 4.4
  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight(Some(Nil): Option[List[A]]){(ao, acc) => 
      acc.flatMap(l => ao.map(a => a :: l))
    }

  // Exercise 4.5
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(Nil): Option[List[B]]) { (a, acc) =>
      acc.flatMap(l => f(a).map(b => b :: l))
    }
}

case class Some[A](get: A) extends Option[A]
case object None extends Option[Nothing]
