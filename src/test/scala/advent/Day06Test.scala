package advent

import org.scalatest._

import Day06._

class Day06Test extends FunSuite {
  val input = List(
    ("COM", "B"),
    ("B", "C"),
    ("C", "D"),
    ("D", "E"),
    ("E", "F"),
    ("B", "G"),
    ("G", "H"),
    ("D", "I"),
    ("E", "J"),
    ("J", "K"),
    ("K", "L")
  )

  test("countOrbits") {
    val tree = makeTree(input)
    assert(countOrbits(tree) == 42)
  }

  test("makeTree") {
    val result = makeTree(input)
    val expected = Tree("COM", List(Tree("B", List(Tree("C", List(Tree("D", List(Tree("E", List(Tree("F", List()), Tree("J", List(Tree("K", List(Tree("L", List()))))))), Tree("I", List()))))), Tree("G", List(Tree("H", List())))))))
    assert(result == expected)
  }

  test("countNodes") {
    val tree = makeTree(input)
    val result = countNodes(tree)
    assert(result == 12)
  }

  val input2 = List(
    ("COM", "B"),
    ("B", "C"),
    ("C", "D"),
    ("D", "E"),
    ("E", "F"),
    ("B", "G"),
    ("G", "H"),
    ("D", "I"),
    ("E", "J"),
    ("J", "K"),
    ("K", "L"),
    ("K", "YOU"),
    ("I", "SAN")
  )

  trait WithTree2 {
    val tree = makeTree(input2)
  }

  test("countOrbitJumps") {
    new WithTree2 {
      val result = countOrbitJumps(tree, "YOU", "SAN")
      assert(result == 4)
    }
  }

  test("findParentNode") {
    val tree = makeTree(input2)
    val Tree(node, _) = findParentNode(tree, "YOU", "SAN")
    assert(node == "D")
  }

  test("jumpsFrom") {
    new WithTree2 {
      val parentNode = findParentNode(tree, "YOU", "SAN")
      val result = jumpsFrom(parentNode, "YOU")
      assert(result == 4)
    }
  }

  test("hasChildNode") {
    val input = Tree("A", List(
      Tree("B", Nil)
    ))

    assert(hasChildNode(input, "B"))
  }
}
