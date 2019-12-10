package advent

import scala.annotation.tailrec

trait OrbitTree

case class Node(value: String, weight: Int, children: Seq[OrbitTree]) extends OrbitTree {
  override def toString(): String =
    s"""Node("$value", $weight, List(${children mkString ", "}))"""
}

case class Leaf(value: String) extends OrbitTree {
  override def toString(): String =
    s"""Leaf("$value")"""
}

object OrbitTree {
  type Entry = (String, String)

  def of (map: Seq[Entry]): OrbitTree = {
    val (startNodes, nextMap) = findChildNodes("COM", map)

    val children = for {
      node <- startNodes
    } yield makeSubTree(node, nextMap)
    val weight = calculateWeight(children)

    Node("COM", weight, children)
  }

  def of(str: String): OrbitTree = {
    val entries = (for {
      line <- str.split('\n')
      if line.size > 0
      value = line.split(')')
    } yield (value(0), value(1))).toList
    of(entries)
  }

  private def makeSubTree (parent: Entry, map: Seq[Entry]): OrbitTree = {
    val (children, nextMap) = findChildNodes(parent._2, map)
    if (children.isEmpty) Leaf(parent._2)
    else {
      val childTrees = for {
        node <- children
      } yield makeSubTree(node, nextMap)
      val weight = calculateWeight(childTrees)
      Node(parent._2, weight, childTrees)
    }
  }

  private def calculateWeight (children: Seq[OrbitTree]): Int =
    children.foldLeft(1) { (prev, x) => x match {
      case Leaf(_) => prev + 1
      case Node(_, weight, _) => prev + weight
    }}

  private def findChildNodes (parent: String, map: Seq[Entry]): (Seq[Entry], Seq[Entry]) =
    map partition {
      case (x, _) if x == parent => true
      case _                     => false
    }
  
  def countOrbits (tree: OrbitTree): Int = {
    tree match {
      case Leaf(_)                   => 0
      case Node(_, weight, children) => {
        val childOrbits = (for {
          node <- children
        } yield countOrbits(node)).sum
        childOrbits + weight - 1
      }
    }
  }
}
