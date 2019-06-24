package fpinscala.ch11

import fpinscala.ch07.Par
import fpinscala.ch08.Gen
import fpinscala.ch09.Parsers

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))
 
  // Exercise 11.3
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(Nil: List[A])) { (ma, acc) =>
      flatMap(ma)(a => map(acc)(as => a :: as))
    }

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(Nil: List[B])) { (a, acc) =>
      flatMap(acc)(bs => map(f(a))(b => b :: bs))
    }

  // Exercise 11.4
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  // Exercise 11.5
  // For list, replicateM constructs lists of length n
  // For option, when we hit first None, we return None otherwise
  // we fold results to one option containing list of all Somes
  // In general we compose all values in F[A] to one monad containing F[List[A]]
  // F defines how values are actually composed

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
    map2(ma, mb)((_, _))

  // Exercise 11.6
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    val mas = traverse(ms) { a => 
      flatMap(f(a)) { b =>
        if (b) unit((true, a)) else unit((false, a))
      }
    }
    map(mas) { as =>
      as.flatMap {
        case (b, a) => if (b) List(a) else Nil
      }
    }
  }
 
  // Exercise 11.7
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(b => g(b))

  // Exercise 11.8
  def flatMap_compose[A, B](fa: F[A])(f: A => F[B]): F[B] =
    compose[Unit, A, B](
      _ => fa,
      a => f(a)
    )(())

  // Exercise 11.9
  //
  // We can use substitution from compose definition to derive associative rule defined by flatMap

  // Exercise 11.10
  //

  // Exercise 11.11
  //
  // compose(f, unit) == f
  //   a => flatMap(f(a))(b => unit(b)) == a => f(a)
  //   flatMap(Some(a))(b => unit(b)) == Some(a)
  //   flatMap(None)(b => unit(b)) == None
  // compose(unit, f) == f
  
  // Exercise 11.12
  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(ma => ma)

  // Exercise 11.13
  def flatMap_joinMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    join(map(fa)(f))

  // Exercise 11.14

  // Exercise 11.15
  //
}

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma.flatMap(f)
  }

  // Exercise 11.1
  val parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)

    def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }

  def parserMonad[P[+_]](implicit parsers: Parsers[P]) = new Monad[P] {
    def unit[A](a: => A): P[A] = parsers.succeed(a)

    def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] =
      parsers.flatMap(ma)(f)
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Option(a)

    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma.flatMap(f)
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)

    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma.flatMap(f)
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)

    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
      ma.flatMap(f)
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st.flatMap(f)
  }

  // Exercise 11.2
  //
  // We have define function that takes type parameter S, so resulting monad defines family of monads one for each State[S, X]
}
