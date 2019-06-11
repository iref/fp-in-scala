package fpinscala.ch10

import fpinscala.ch02.Chapter2
import fpinscala.ch03.{Tree, Leaf, Branch}

trait Foldable[F[_]] {
    def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B = {
        val mapper = Chapter2.curry(f)
        val reducers = foldMap[A, B => B](as)(mapper)(Monoid.endoMonoid)
        reducers(z)
    }
    
    def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B = {
        val mapper = (a: A) => (b: B) => f(b, a)
        val reducers = foldMap[A, B => B](as)(mapper)(Monoid.endoMonoid)
        reducers(z)
    }
    
    def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B

    def concatenate[A](as: F[A])(m: Monoid[A]): A =
        foldLeft(as)(m.zero)(m.op)

    // Exercise 10.15
    def toList[A](as: F[A]): List[A] =
        foldRight(as)(Nil: List[A])((a, as) => a :: as)
}
    
object Foldable {
    // Exercise 10.12
    val listFoldable: Foldable[List] =
        new Foldable[List] {
            override def foldRight[A,B](as: List[A])(z: B)(f: (A,B) => B): B =
                as.foldRight(z)(f)
    
            override def foldLeft[A,B](as: List[A])(z: B)(f: (B,A) => B): B =
                as.foldLeft(z)(f)
    
            def foldMap[A,B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
                Chapter10.foldMap(as, mb)(f)
        }

    val streamFoldable: Foldable[Stream] =
        new Foldable[Stream] {
            override def foldRight[A,B](as: Stream[A])(z: B)(f: (A,B) => B): B =
                as.foldRight(z)(f)
    
            override def foldLeft[A,B](as: Stream[A])(z: B)(f: (B,A) => B): B =
                as.foldLeft(z)(f)
            
            def foldMap[A,B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
                foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
        }

    val indexedSeqFoldable: Foldable[IndexedSeq] =
        new Foldable[IndexedSeq] {
            def foldMap[A,B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
                Chapter10.foldMapV(as, mb)(f)
        }

    // Exercise 10.13
    val treeFoldable: Foldable[Tree] =
        new Foldable[Tree] {
            override def foldRight[A,B](as: Tree[A])(z: B)(f: (A,B) => B): B =
                as match {
                    case Leaf(a) => f(a, z)
                    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
                }
    
            override def foldLeft[A,B](as: Tree[A])(z: B)(f: (B,A) => B): B =
                as match {
                    case Leaf(a) => f(z, a)
                    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
                }
            
            def foldMap[A,B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
                Tree.fold(as)(f)(mb.op)
        }

    // Exercise 10.14
    val optionFoldable: Foldable[Option] =
        new Foldable[Option] {
            override def foldRight[A,B](as: Option[A])(z: B)(f: (A,B) => B): B =
                as match {
                    case None => z
                    case Some(a) => f(a, z)
                }

            override def foldLeft[A,B](as: Option[A])(z: B)(f: (B,A) => B): B =
                as match {
                    case None => z
                    case Some(a) => f(z, a)
                }

            override def foldMap[A,B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
                foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
        }
}
