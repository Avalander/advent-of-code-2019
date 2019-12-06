package advent

import IntComputer._
import scala.io.Source

object Day05 {
  def readInput (): Program = {
    val input = Source.fromResource("day-05.txt").getLines()
    (for {
      line <- input
      value <- line.split(',')
    } yield value.toInt).toVector
  }

  def problem1 (program: Program): Unit = {
    val inputs = List(1)
    val (result, output) = runExtended(program, inputs = inputs)
    println(s"Output: $output")
  }

  def problem2 (program: Program): Unit = {
    val inputs = List(5)
    val (result, output) = runExtended(program, inputs = inputs)
    println(s"Output: $output")
  }

  def main (args: Array[String]): Unit = {
    val input = readInput()

    problem1(input)
    problem2(input)
  }
}
