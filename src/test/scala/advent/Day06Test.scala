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
}
