package fpinscala.ch07.actor

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

sealed trait Future[+A] {
    private[actor] def apply(cb: A => Unit, onError: Throwable => Unit): Unit
}

object Par {
    type Par[A] = ExecutorService => Future[A]

    def run[A](par: Par[A])(es: ExecutorService): Either[Throwable, A] = {
        val ref = new AtomicReference[Either[Throwable, A]]()

        val latch = new CountDownLatch(1)

        val onSuccess = (a: A) => {
            ref.set(Right(a))
            latch.countDown()
        }
        val onError = (exp: Throwable) => {
            ref.set(Left(exp))
            latch.countDown()
        }

        par(es)(onSuccess, onError)

        latch.await()

        ref.get
    }

    def unit[A](value: A): Par[A] =
        _ => new Future[A] {
            def apply(cb: A => Unit, onError: Throwable => Unit): Unit = cb(value)
        }

    def map2[A, B, C](parA: Par[A], parB: Par[B])(f: (A, B) => C): Par[C] =
        es => new Future[C] {
            def apply(cb: C => Unit, onError: Throwable => Unit): Unit = {
                var resultA: Option[A] = None
                var resultB: Option[B] = None

                val handler: Either[A, B] => Unit = {
                    case Left(a) =>
                        resultB match {
                            case Some(b) => cb(f(a, b))
                            case None => resultA = Some(a)
                        }

                    case Right(b) =>
                        resultA match {
                            case Some(a) => cb(f(a, b))
                            case None => resultB = Some(b)
                        }

                }
                val combiner = Actor(es)(handler, onError)

                parA(es)(a => combiner ! Left(a), onError)
                parB(es)(b => combiner ! Right(b), onError)
            }
        }

    def fork[A](value: => Par[A]): Par[A] =
        es => new Future[A] {
            def apply(cb: A => Unit, onError: Throwable => Unit): Unit =
                eval(es)(value(es)(cb, onError), onError)
        }

    private def eval(es: ExecutorService)(r: => Unit, onError: Throwable => Unit): Unit =
        es.submit(new Callable[Unit] {
            def call: Unit =
                try {
                    r
                } catch {
                    case exp: Throwable => onError(exp)
                }
        })

    def lazyUnit[A](value: => A): Par[A] = fork(unit(value))

    def asyncF[A,B](f: A => B): A => Par[B] =
        a => lazyUnit(f(a))

    def map[A, B](parA: Par[A])(f: A => B): Par[B] =
        map2(parA, unit(()))((a, _) => f(a))

    def sortPar(ints: Par[List[Int]]): Par[List[Int]] =
        map(ints)(ints => ints.sorted)

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
        val parAs = ps.map(asyncF(f))
        sequence(parAs)
    }

    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
        ps.foldRight(unit(Nil: List[A])) { (parA, acc) =>
            map2(parA, acc)(_ :: _)
        }

    def parFilter[A](ps: List[A])(f: A => Boolean): Par[List[A]] = fork {
        val filterPars = ps.map(
            asyncF { a => if (f(a)) List(a) else Nil }
        )
        map(sequence(filterPars))(_.flatten)
    }
}