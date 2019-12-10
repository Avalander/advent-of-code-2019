package advent

import org.scalatest._

import OrbitTree._

class OrbitTreeTest extends FunSuite {
  trait withInputs {
    val seqInput = List(
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
    )
    val strInput = seqInput map ((x) => x._1 ++ ")" ++ x._2) mkString "\n"
    val expected = Node("COM", 12, List(Node("B", 11, List(Node("C", 8, List(Node("D", 7, List(Node("E", 5, List(Leaf("F"), Node("J", 3, List(Node("K", 2, List(Leaf("L"))))))), Leaf("I"))))), Node("G", 2, List(Leaf("H")))))))
  }

  trait withTree extends withInputs {
    val tree = of(seqInput)
  }

  test("Makes tree from string input") {
    new withInputs {
      val result = of(strInput)
      assert(result == expected)
    }
  }

  test("Makes tree from sequence input") {
    new withInputs {
      val result = of(seqInput)
      assert(result == expected)
    }
  }

  test("countOrbits") {
    new withTree {
      assert(countOrbits(tree) == 42)
    }
  }
}
