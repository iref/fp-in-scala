package fpinscala.ch03

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](a: A) extends Tree[A]


object Tree {
  // Exercise 3.25
  def size[A](tree: Tree[A]): Int =
    tree match {
      case Branch(left, right) => 1 + size(left) + size(right)
      case Leaf(_) => 1
    }

  // Exercise 3.26
  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(i) => i
      case Branch(left, right) => maximum(left).max(maximum(right))
    }

  // Exercise 3.27
  def depth[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + depth(left).max(depth(right))
    }

  // Exercise 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

  // Exercise 3.29
  def fold[A, B](tree: Tree[A])(zero: A => B, branch: (B, B) => B): B =
    tree match {
      case Leaf(a) => zero(a)
      case Branch(left, right) => branch(fold(left)(zero, branch), fold(right)(zero, branch))
    }

  def size2[A](tree: Tree[A]): Int =
    fold[A, Int](tree)(_ => 1, (l, r) => 1 + l + r)

  def maximum2(tree: Tree[Int]): Int =
    fold[Int, Int](tree)(a => a, (l, r) => l.max(r))

  def depth2[A](tree: Tree[A]): Int =
    fold[A, Int](tree)(_ => 0, (l, r) => 1 + l.max(r))

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](tree)(a => Leaf(f(a)), (l, r) => Branch(l, r))

}
