package fpinscala.ch10

import java.util.concurrent.Executors
import fpinscala.AbstractSpec
import fpinscala.ch07.actor.Par
import fpinscala.ch08._

class Chapter10Spec extends AbstractSpec {

    "foldMap_withFoldLeft" should "correctly accumulate elements of list" in {
        val list = List(1, 2, 3, 4)
        val result = Chapter10.foldMap_byFoldLeft(list, Monoid.intAdditionMonoid) { a =>
            a + 2
        }

        result should be(18)
    }

    "foldMap" should "correctly accumulate elements of list" in {
        val list = List(1, 2, 3, 4)
        val result = Chapter10.foldMap(list, Monoid.intAdditionMonoid) { a =>
            a + 2
        }

        result should be(18)
    }

    "foldLeft_byFoldMap" should "behave as foldLeft on list" in {
        val list = List(1, 2, 3, 4)
        val acc = (acc: Int, a: Int) => acc + a + 2
        val expected = list.foldLeft(0)(acc)
        val result = Chapter10.foldLeft_byFoldMap(list, 0)(acc)

        result should be(expected)
    }

    "foldRight_byFoldMap" should "behave as foldRight on list" in {
        val list = List(1, 2, 3, 4)
        val acc = (acc: Int, a: Int) => acc - a - 1
        val expected = list.foldLeft(20)(acc)
        val result = Chapter10.foldLeft_byFoldMap(list, 20)(acc)

        result should be(expected)
    }

    "foldMapV" should "correctly accumulate elements of indexed seq" in {
        val vec = Vector(1, 2, 3, 4)
        val result = Chapter10.foldMapV(vec, Monoid.intAdditionMonoid) { a =>
            a + 2
        }

        result should be(18)
    }

    "parFoldMap" should "correctly accumulate elements of indexed seq" in {
        val vec = Vector(1, 2, 3, 4)
        val resultPar = Chapter10.parFoldMap(vec, Monoid.intAdditionMonoid) { a =>
            a + 2
        }
        val result = Par.run(resultPar)(Executors.newFixedThreadPool(2))

        result.right.value should be(18)
    }

    "wordCount" should "correctly count words in string" in {
        val prop = Prop.forAll(Gen.sentences) { sentence =>
            val expected = sentence.split(" ").size
            val actual = Chapter10.wordCount(sentence)
            actual == expected
        }
        Prop.run(prop)
    }
}
