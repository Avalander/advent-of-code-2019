package advent

import scala.io.Source

object Day03 {
  case class Point(x: Int, y: Int)

  def closestIntersection (a: Seq[String], b: Seq[String]): Int = {
    val pathA = toPoints(a, Point(0, 0))
    val pathB = toPoints(b, Point(0, 0))

    val distances = pathA.intersect(pathB).tail map {
      case Point(x, y) => math.abs(x) + math.abs(y)
    }

    distances.min
  }

  def closestInSteps (a: Seq[String], b: Seq[String]): Int = {
    val pathA = toPoints(a, Point(0, 0))
    val pathB = toPoints(b, Point(0, 0))

    val steps = pathA.intersect(pathB).tail map {
      point => pathA.indexOf(point) + pathB.indexOf(point)
    }

    steps.min
  }

  private def toPoints (xs: Seq[String], start: Point): Seq[Point] = {
    val vectors = for {
      x <- xs
      (dir :: rest) = x.toList
      length = rest.mkString.toInt
      i <- (1 to length)
    } yield dir match {
      case 'R' => Point(1, 0)
      case 'L' => Point(-1, 0)
      case 'U' => Point(0, 1)
      case 'D' => Point(0, -1)
    }

    vectors.foldLeft(Vector(start)) {
      case (result, Point(x2, y2)) => {
        val Point(x, y) = result.last
        val next = Point(x + x2, y + y2)
        result :+ next
      }
    }
  }

  def readInput (): (Seq[String], Seq[String]) = {
    val input = Source.fromResource("day-03.txt").getLines()
    val lines = (for {
      line <- input
    } yield line.split(',')).toList
    (lines(0), lines(1))
  }

  def main (args: Array[String]): Unit = {
    val (a, b) = readInput()
    val result1 = closestIntersection(a, b)
    println(s"Problem 1: $result1")

    val result2 = closestInSteps(a, b)
    println(s"Problem 2: $result2")
  }
}
