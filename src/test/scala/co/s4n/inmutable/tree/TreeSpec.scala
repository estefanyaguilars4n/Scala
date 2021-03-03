package co.s4n.inmutable.tree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TreeSpec extends AnyFlatSpec with Matchers{
  "La función size enviándole el árbol Leaf(10)" should "debería devolver 1" in {
    Tree.size(Leaf(10)) shouldBe(1)
  }
  "La función size enviándole el árbol Branch(Leaf(10),Leaf(20))" should "debería devolver 1" in {
    Tree.size(Branch(Leaf(10),Leaf(20))) shouldBe(3)
  }
}
