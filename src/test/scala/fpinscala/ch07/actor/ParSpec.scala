package fpinscala.ch07.actor

import java.util.concurrent.{Executors, ExecutorService}
import org.scalatest.exceptions.TestFailedException
import fpinscala.AbstractSpec

class ParSpec extends AbstractSpec {

    def run(test: ExecutorService => Unit): Unit = {
        val es = Executors.newSingleThreadExecutor()

        try {
            test(es)
        } catch {
            case failedTest: TestFailedException => throw failedTest
            case e: Throwable => fail(e)
        } finally {
            es.shutdown()
        }
    }

    "run" should "complete with correct value" in {
        run { es =>
            val p = Par.lazyUnit(1)
            Par.run(p)(es).right.value should be(1)
        }
    }

    it should "correctly return exceptions" in {
        run { es =>
            val actual = Par.map2(Par.unit(2), Par.lazyUnit(3)) { (_, _) => throw new IllegalArgumentException("test") }

            Par.run(actual)(es).left.value shouldBe a [IllegalArgumentException]
        }
    }

    "fork" should "return same result as original par" in {
        run { es =>
            val p = Par.unit(2)
            val forked = Par.fork(p)

            Par.run(forked)(es) should be(Par.run(p)(es))
        }
    }

    "map2" should "return correct value" in {
        run { es =>
            val pa = Par.lazyUnit(1 + 2)
            val pb = Par.lazyUnit(3 + 4)
            val pc = Par.map2(pa, pb) { (a, b) =>
                a * b
            }

            Par.run(pc)(es).right.value should be(21)
        }
    }

    "lazyUnit" should "return same result as fork(unit(x))" in {
        run { es =>
            val lazyP = Par.lazyUnit(2)
            val forkedUnit = Par.fork(Par.unit(2))

            Par.run(lazyP)(es) should be(Par.run(forkedUnit)(es))
        }
    }

    "map" should "obey mapping law: map(unit(x))(f) == unit(f(x))" in {
        run { es => 
            val pa = Par.lazyUnit(2)
            val result = Par.map(pa) { a => a + 3 }

            val expected = Par.unit(2 + 3)

            Par.run(result)(es) should be(Par.run(expected)(es))
        }
    }
}