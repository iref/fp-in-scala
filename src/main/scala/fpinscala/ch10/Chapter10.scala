package fpinscala.ch10

import fpinscala.ch02.Chapter2
import fpinscala.ch07.actor.Par._

object Chapter10 {

    // Exercise 10.5
    def foldMap_byFoldLeft[A, B](as: List[A], monoid: Monoid[B])(f: A => B): B =
        as.foldLeft(monoid.zero) { (acc, a) =>
            monoid.op(acc, f(a))
        }

    // Exercise 10.6
    def foldMap[A, B](as: List[A], monoid: Monoid[B])(f: A => B): B = {
        @scala.annotation.tailrec
        def step(as: List[A], acc: B): B =
            as match {
                case Nil => acc
                case x :: xs => step(xs, monoid.op(f(x), acc))
            }

        step(as, monoid.zero)
    }

    def foldLeft_byFoldMap[A, B](as: List[A], b: B)(f: (B, A) => B): B = {
        val mapper = (a: A) => (b: B) => f(b, a)
        val reducers = foldMap[A, B => B](as, Monoid.endoMonoid) { a =>
            mapper(a)
        }
        reducers(b)
    }

    def foldRight_byFoldMap[A, B](as: List[A], b: B)(f: (A, B) => B): B = {
        val mapper = Chapter2.curry(f)
        val reducers = foldMap[A, B => B](as, Monoid.endoMonoid) { a =>
            mapper(a)
        }
        reducers(b)
    }

    // Exercise 10.7
    def foldMapV[A, B](as: IndexedSeq[A], monoid: Monoid[B])(f: A => B): B = {
        if (as.isEmpty) {
            monoid.zero
        } else if (as.size == 1) {
            monoid.op(f(as.head), monoid.zero)
        } else {
            val (left, right) = as.splitAt(as.size / 2)
            monoid.op(
                foldMapV(left, monoid)(f),
                foldMapV(right, monoid)(f)
            )
        }
    }

    // Exercise 10.8
    def par[A](m: Monoid[A]): Monoid[Par[A]] =
        new Monoid[Par[A]] {
            val zero = unit(m.zero)
            def op(left: Par[A], right: Par[A]): Par[A] =
                map2(left, right)(m.op)
        }

    def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
        val parMonoid = par(m)
        if (v.isEmpty) {
            parMonoid.zero
        } else if (v.size == 1) {
            parMonoid.op(
                lazyUnit(f(v.head)),
                parMonoid.zero
            )
        } else {
            val (left, right) = v.splitAt(v.size / 2)
            parMonoid.op(
                parFoldMap(left, m)(f),
                parFoldMap(right, m)(f)
            )
        }
    }

    // Exercise 10.11
    def wordCount(string: String): Int = {
        def wc(c: Char): WC =
            if (c.isWhitespace) {
                Part("", 0, "")
            } else {
                Stub(c.toString)
            }
        def count(s: String): Int = if (s.isEmpty) 0 else 1

        foldMapV(string.toIndexedSeq, Monoid.wordCountMonoid)(wc) match {
            case Stub(s) => count(s)
            case Part(l, wc, r) => count(l) + wc + count(r)
        }
    }
}
