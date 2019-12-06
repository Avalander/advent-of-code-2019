package advent

import scala.io.Source

object Day06 {
  type Node = (String, String)

  case class Tree(value: String, children: Seq[Tree]) {
    override def toString(): String =
      s"Tree($value, [${children mkString ", "}])"
  }

  def makeTree (map: Seq[Node]): Tree = {
    val (startNodes, nextMap) = findChildNodes("COM", map)

    val children = for {
      node <- startNodes
    } yield makeSubTree(node, nextMap)
    Tree("COM", children)
  }

  private def makeSubTree (parent: Node, map: Seq[Node]): Tree = {
    val (children, nextMap) = findChildNodes(parent._2, map)
    if (children.isEmpty) Tree(parent._2, List())
    else {
      val childTrees = for {
        node <- children
      } yield makeSubTree(node, nextMap)
      Tree(parent._2, childTrees)
    }
  }

  private def findChildNodes (parent: String, map: Seq[Node]): (Seq[Node], Seq[Node]) =
    map partition {
      case (x, _) if x == parent => true
      case _                     => false
    }
  
  def countOrbits (tree: Tree): Int = {
    val Tree(_, children) = tree
    if (children.isEmpty) 0
    else {
      (for {
        node <- children
      } yield countOrbits(node)).sum + countNodes(tree) - 1
    }
  }

  def countNodes (tree: Tree): Int = {
    val Tree(_, children) = tree
    if (children.isEmpty) 1
    else children.map(countNodes).sum + 1
  }
  
  def readInput (): Seq[Node] = {
    val input = Source.fromResource("day-06.txt").getLines()
    (for {
      line <- input
      if line.size > 0
      value = line.split(')')
    } yield (value(0), value(1))).toList
  }
  
  def main (args: Array[String]): Unit = {
    val input = readInput()

    val tree = makeTree(input)
    println(tree)

    println(countOrbits(tree))
  }
}
