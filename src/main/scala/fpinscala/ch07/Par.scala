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

    // Exercise 7.7

    // Simplification
    //
    // map(unit(x))(f) == unit(x)(f)
    // map(unit(x))(id) == unit(id(x))
    // map(unit(x))(id) == unit(x)
    // map(y)(id) == y
    //
    //
    // map(map(y)(g))(f) == map(y)(f compose g)
    // map(map(y)(g))(f) == 

    // Exercise 7.8
    //
    // If we choose Executors.newSingleThreadExecutor()
    // and use fork(fork(unit(1)))...
    // es => es.submit(new Callable[A] {
    //        def call: A = fork(unit(1))(es).get
    //    })
    // This starts new task in our single thread, that tries to submit
    // new task (fork(unit(1))) in same thread pool, but Executor
    // doesn't have free thread to assign to the second task.
    // Second task waits on first task to finish, so it can receive thread
    // from thread pool. First task waits on second task to finish, so it 
    // can release thread back to thread pool => deadlock

    // Exercise 7.9
    //
    // For fixed executor service of size N.
    //
    // val tasks = (0..N).toList
    // val computation = tasks.foldLeft(lazyUnit(1)) { (acc, _) =>
    //    fork(task)
    // }
    // This code spawns N+1 nested computations, which means that there is always
    // at least one computation that has to wait for free thread while first N
    // computations wait on it to finish => deadlock

    def delay[A](fa: => Par[A]): Par[A] =
        Par(es => fa.run(es))

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
        Par(es => 
            if (cond.run(es).get) {
                t.run(es)
            } else {
                f.run(es)
            }
        )

    // Exercise 7.11
    def choiceN[A](n: Par[Int])(list: List[Par[A]]): Par[A] =
        // Doesn't handle case when i is out of bounds
        Par(es => {
            val i = n.run(es).get
            list(i).run(es)
        })

    // Exercise 7.12
    def choiceMap[K, V](kPar: Par[K])(map: Map[K, Par[V]]): Par[V] =
        Par(es => {
            val k = kPar.run(es).get
            map(k).run(es)
        })

    // Exercise 7.13
    def flatMap[A, B](parA: Par[A])(f: A => Par[B]): Par[B] =
        Par(es => {
            val a = parA.run(es).get
            f(a).run(es)
        })

    // Exercise 7.14
    def join[A](par: Par[Par[A]]): Par[A] =
        Par(es => par.run(es).get.run(es))

    def flatMap_join[A, B](par: Par[A])(f: A => Par[B]): Par[B] = {
        val mapped = Par.map(par)(f)
        Par.join(mapped)
    }

    def join_flatMap[A](par: Par[Par[A]]): Par[A] =
        flatMap(par)(parA => parA)

    def choice_flatMap[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
        flatMap(cond) { pred => if (pred) t else f }

    def choiceN_flatMap[A](n: Par[Int])(list: List[Par[A]]): Par[A] =
        flatMap(n) { i => list(i) }

    def choiceMap_flatMap[K, V](kPar: Par[K])(map: Map[K, Par[V]]): Par[V] =
        flatMap(kPar) { k => map(k) }

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