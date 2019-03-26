package fpinscala.ch07

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}
import java.util.concurrent.atomic.AtomicReference

// Exercise 7.2
case class Par[A](run: ExecutorService => Future[A])

object Par {

    def unit[A](value: A): Par[A] = Par(_ => UnitFuture(value))
        
    def run[A](es: ExecutorService)(par: Par[A]): Future[A] = par.run(es)

    // Exercise 7.1
    def map2[A, B, C](parA: Par[A], parB: Par[B])(f: (A, B) => C) = 
        Par(es => {
            val a = parA.run(es)
            val b = parB.run(es)
            Map2Future(a, b, f)
        })

    def fork[A](value: => Par[A]): Par[A] =
        Par(es => es.submit(new Callable[A] {
            def call: A = value.run(es).get
        }))

    def lazyUnit[A](value: => A): Par[A] = fork(unit(value))

    // Exercise 7.4
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

    // Exercise 7.5
    //
    // Not really efficient, we can paralelize joining
    // similarly to sum example at the beginning of the chapter
    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
        ps.foldRight(unit(Nil: List[A])) { (parA, acc) =>
            map2(parA, acc)(_ :: _)
        }

    // Exercise 7.6
    def parFilter[A](ps: List[A])(f: A => Boolean): Par[List[A]] = fork {
        val filterPars = ps.map(
            asyncF { a => if (f(a)) List(a) else Nil }
        )
        map(sequence(filterPars))(_.flatten)
    }


    private case class UnitFuture[A](get: A) extends Future[A] {
        def isDone: Boolean = true
        def get(timeout: Long, units: TimeUnit): A = get
        def isCancelled: Boolean = false
        def cancel(evenIfRunning: Boolean): Boolean = false
    }

    // Exercise 7.3
    private case class Map2Future[A, B, C](
        futureA: Future[A], 
        futureB: Future[B],
        f: (A, B) => C
    ) extends Future[C] {
        private val cache: AtomicReference[C] = new AtomicReference()

        def isDone: Boolean = cache.get != null

        def get: C = get(Long.MaxValue, TimeUnit.MILLISECONDS)

        def get(timeout: Long, units: TimeUnit): C = {
            if (cache.get != null) {
                cache.get
            } else {
                val result = run(timeout, units)
                cache.set(result)
                result
            }
        }

        def isCancelled: Boolean = futureA.isCancelled || futureB.isCancelled

        def cancel(evenIfRunning: Boolean): Boolean = {
            val cancelationA = futureA.cancel(evenIfRunning)
            val cancelationB = futureB.cancel(evenIfRunning)
            cancelationA || cancelationB
        }

        private def run(timeout: Long, units: TimeUnit): C = {
            val total = TimeUnit.MILLISECONDS.convert(timeout, units)
            val startAt = System.currentTimeMillis

            val a = futureA.get(timeout, units)
            
            val remaining = total - (System.currentTimeMillis - startAt)

            val b = futureB.get(remaining, TimeUnit.MILLISECONDS)

            f(a, b)
        }
    }
}