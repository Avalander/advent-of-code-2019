package advent

import scala.io.Source
import scala.annotation.tailrec

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
  

  // Problem 1

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
  

  // Problem 2

  def countOrbitJumps (tree: Tree, from: String, to: String): Int = {
    val parent = findParentNode(tree, from, to)
    jumpsFrom(parent, from) + jumpsFrom(parent, to) - 2
  }

  @tailrec
  def findParentNode (tree: Tree, from: String, to: String): Tree = {
    val Tree(_, children) = tree
    val nextNode = children find (x => hasChildNode(x, from) && hasChildNode(x, to))
    nextNode match {
      case None        => tree
      case Some(value) => findParentNode(value, from, to)
    }
  }

  def hasChildNode (tree: Tree, value: String): Boolean =
    tree match {
      case Tree(_, Nil) => false
      case Tree(_, children) => {
        val isDirectChild = children exists {
          case Tree(x, _) => x == value
        }
        isDirectChild || children.exists(hasChildNode(_, value))
      }
    }
  
  @tailrec
  def jumpsFrom (tree: Tree, to: String, total: Int = 0): Int = {
    val Tree(x, children) = tree
    if (x == to) total
    else {
      val nextNode = children.find(hasChildNode(_, to))
      nextNode match {
        // None means to is direct descendant a child node
        // need to figure out why hasChildNode doesn't find it.
        case None => total + 1
        case Some(value) => jumpsFrom(value, to, total + 1)
      }
    }
  }


  // Main

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

    val result1 = countOrbits(tree)
    println(s"Problem 1: $result1")

    val result2 = countOrbitJumps(tree, "YOU", "SAN")
    println(s"Problem 2: $result2")
  }
}
