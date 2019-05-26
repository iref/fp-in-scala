package fpinscala.ch05

sealed trait Stream[+A] {

  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n-1))
      case _ => Stream.empty
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

  def takeWhile(f: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if f(h()) => Stream.cons(h(), t().takeWhile(f))
      case _ => Stream.empty
    }

  def exists(p: A => Boolean): Boolean =
    this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case Empty => z
    }

  def exist_foldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile_foldRight(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A]) { (a, b) =>
      if (f(a)) Stream.cons(a, b) else Stream.empty
    }

  def headOption: Option[A] =
    this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

  def headOption_foldRight: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B]) { (a, b) =>
      Stream.cons(f(a), b)
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A]) { (a, b) =>
      if (p(a)) Stream.cons(a,b) else b
    }

  def append[AA >: A](s: => Stream[AA]): Stream[AA] =
    foldRight(s) { (a, b) =>
      Stream.cons(a, b)
    }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B]) { (a ,b) =>
      f(a).append(b)
    }

  def map_unfold[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def take_unfold(n: Int): Stream[A] =
    Stream.unfold((n, this)) {
      case (0, _) => None
      case (n, Empty) => None
      case (n, Cons(h, t)) if n > 0 => Some((h(), (n - 1, t())))
    }

  def takeWhile_unfold(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => {
        val c = f(h1(), h2())
        val s = (t1(), t2())
        Some((c, s))
      }
      case _ => None
    }

  def zip[B](s: Stream[B]): Stream[(A, B)] =
    zipWith(s)((a, b) => (a, b))

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => {
        val c = (Some(h1()), Some(h2()))
        val s = (t1(), t2())
        Some((c, s))
      }

      case (Cons(h1, t1), Empty) => {
        val c = (Some(h1()), None)
        val s = (t1(), Empty)
        Some((c, s))
      }

      case (Empty, Cons(h, t)) => {
        val c = (None, Some(h()))
        val s = (Empty, t())
        Some((c, s))
      }

      case (Empty, Empty) => None
    }

  def tails: Stream[Stream[A]] =
    Stream.unfold(this) { s =>
      s match {
        case Cons(_, t) => Some((s, t()))
        case Empty => None
      }
    } append Stream(Stream.empty)

  def find(f: A => Boolean): Option[A] =
    this match {
      case Cons(a, t) => if (f(a())) Some(a()) else t().find(f)
      case Empty => None
    }

  /* def scanRight[B](z: B)(f: (A, B) => B): Stream[B] =
    Stream.unfold(this) { s =>
      s match {
        case Cons(h, t) => Some((t, f(h, t())))
        case Empty => None
      }
    } append Stream(z)
  */

}
case object Empty extends Stream[Nothing]
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Long] = {
    def go(f1: Long, f2: Long): Stream[Long] = {
      cons(f1, go(f2, f1 + f2))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

  def ones_unfold: Stream[Int] =
    unfold(())(_ => Some((1, ())))

  def constant_unfold[A](a: A): Stream[A] =
    unfold(())(_ => Some((a, ())))

  def from_unfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n+1)))

  def fibs_unfold: Stream[Long] =
    unfold((0, 1)) { case (a, b) =>
      Some((a, (b, a+1)))
    }
}

