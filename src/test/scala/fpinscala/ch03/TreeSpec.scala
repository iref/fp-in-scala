package fpinscala.ch03

import fpinscala.AbstractSpec

class TreeSpec extends AbstractSpec {
    val tree = Branch(
      Branch(
        Branch(
          Leaf(1),
          Leaf(2)
        ),
        Leaf(3)
      ),
      Branch(
        Branch(
          Branch(
            Leaf(4), Leaf(5)
          ),
          Leaf(6),
        ),
        Leaf(7),
      )
    )

  "size" should "be equal to node count in tree" in {
    Tree.size(tree) should be(13)
  }

  "depth" should "be equal to greatest number of edges between root and leaf" in {
    Tree.depth(tree) should be(4)
  }

  "maximum" should "be equal to the greatest value stored in leaf" in {
    Tree.maximum(tree) should be(7)
  }

  "map" should "transform value in each leaf and preserve tree structure" in {
    val original = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val expected = Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(4), Leaf(5)))

    val actual = Tree.map(original)(a => a + 1)

    actual should be(expected)
  }
}
