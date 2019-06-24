package fpinscala.ch10

trait Monoid[A] {
    def zero: A

    def op(left: A, right: A): A
}

object Monoid {
    // Exercise 10.1
    val intAdditionMonoid: Monoid[Int] =
        new Monoid[Int] {
            val zero = 0
            def op(left: Int, right: Int): Int = left + right
        }

    val intMultiplicationMonoid: Monoid[Int] =
        new Monoid[Int] {
            val zero = 1
            def op(left: Int, right: Int): Int = left * right
        }

    val booleanOrMonoid: Monoid[Boolean] =
        new Monoid[Boolean] {
            val zero = false
            def op(left: Boolean, right: Boolean): Boolean = left || right
        }

    val booleanAndMonoid: Monoid[Boolean] =
        new Monoid[Boolean] {
            val zero = true
            def op(left: Boolean, right: Boolean): Boolean = left && right
        }
    
    // Exercise 10.2
    def optionMonoid[A]: Monoid[Option[A]] =
        new Monoid[Option[A]] {
            val zero = None
            def op(left: Option[A], right: Option[A]): Option[A] =
                left orElse right
        }

    // Exercise 10.3
    def endoMonoid[A]: Monoid[A => A] =
        new Monoid[A => A] {
            val zero = identity
            def op(left: A => A, right: A => A): A => A =
                left.andThen(right)
        }

    val stringMonoid: Monoid[String] =
        new Monoid[String] {
            val zero = ""
            def op(left: String, right: String): String = f"$left$right"
        }

    def listMonoid[A]: Monoid[List[A]] =
        new Monoid[List[A]] {
            val zero: List[A] = Nil
            def op(left: List[A], right: List[A]): List[A] =
                left ++ right
        }

    // Exercise 10.9
    val sortedIndexedSeqMonoid: Monoid[IndexedSeq[Int]] =
        new Monoid[IndexedSeq[Int]] {
            val zero: IndexedSeq[Int] = Vector()
            def op(left: IndexedSeq[Int], right: IndexedSeq[Int]): IndexedSeq[Int] =
                ???
        }

    // Exercise 10.10
    val wordCountMonoid: Monoid[WC] =
        new Monoid[WC] {
            val zero: WC = Stub("")
            def op(left: WC, right: WC): WC =
                (left, right) match {
                    case (Stub(ls), Stub(rs)) => Stub(f"$ls$rs")
                    case (Stub(ls), Part(l, wc, r)) => Part(f"$ls$l", wc, r)
                    case (Part(l, wc, r), Stub(rs)) => Part(l, wc, f"r$rs")
                    case (Part(l1, wc1, r1), Part(l2, wc2, r2)) =>
                        val commonWordCount = if (r1.isEmpty && l1.isEmpty) 0 else 1
                        Part(l1, wc1 + wc2 + commonWordCount , r2)
                }
        }

    // Exercise 10.16
    def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] =
        new Monoid[(A, B)] {
            val zero = (ma.zero, mb.zero)
            def op(left: (A, B), right: (A, B)): (A, B) =
                (
                    ma.op(left._1, right._1),
                    mb.op(left._2, right._2)
                )
        }

    def mapMergeMonoid[K, V](monoidV: Monoid[V]): Monoid[Map[K, V]] =
      new Monoid[Map[K, V]] {
        val zero = Map[K, V]()
        def op(left: Map[K, V], right: Map[K, V]): Map[K, V] =
          (left.keySet ++ right.keySet).foldLeft(zero) { (acc, k) =>
            acc.updated(
              k,
              monoidV.op(
                left.getOrElse(k, monoidV.zero),
                right.getOrElse(k, monoidV.zero)
              )
            )
          }
      }

    // Exercise 10.17
    def functionMonoid[A, B](monoid: Monoid[B]): Monoid[A => B] =
      new Monoid[A => B] {
        val zero = (a: A) => monoid.zero
        def op(left: A => B, right: A => B): A => B =
          (a: A) => monoid.op(left(a), right(a))
      }
}
