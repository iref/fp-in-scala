package fpinscala.ch08

object Chapter8 {

    // Exercise 8.2
    // 1. maximum element of non-empty list is >= that every element in the list
    // 2. maximum element of empty list  should throw exception or return some default value
    // 3. maximum element of single-element list is equal to that element

    // Exercise 8.13
    val smallInt = Gen.choose(-10, 10)
    val maxProp = Prop.forAll(SGen.listOf(smallInt)) { ns =>
        val max = ns.max
        !ns.exists(_ > max)
    }

    // Exercise 8.14
    val sortedListProp = Prop.forAll(SGen.listOf(smallInt)) { ns =>
        val sorted = ns.sorted
        // sorted.zip(sorted.tail).forall { case (a, b) => a <= b }
        ns.forall { n => sorted.contains(n) }
    }

}