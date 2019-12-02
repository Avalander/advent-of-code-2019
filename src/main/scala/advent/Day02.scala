package advent

import scala.annotation.tailrec
import scala.io.Source

object Day02 {
  type Program = Vector[Int]

  @tailrec
  def run (program: Program, index: Int = 0): Program =
    program.slice(index, index + 4).toList match {
      case (99 :: xs)          => program
      case List(1, a, b, dest) => {
        val value = program(a) + program(b)
        val result = program.patch(dest, List(value), 1)
        run(result, index + 4)
      }
      case List(2, a, b, dest) => {
        val value = program(a) * program(b)
        val result = program.patch(dest, List(value), 1)
        run(result, index + 4)
      }
  }

  def calculateInputFor (program: Program, target: Int): (Int, Int) = {
    val result = for {
      noun <- (0 to 99)
      verb <- (0 to 99)
      patched = program.patch(1, List(noun, verb), 2)
      output = run(patched)
      if output(0) == target
    } yield (noun, verb)
    result(0)
  }

  def readInput (): Program = {
    val input = Source.fromResource("day-02.txt").getLines()
    (for {
      line <- input
      value <- line.split(',')
    } yield value.toInt).toVector.patch(1, List(12, 2), 2)
  }

  implicit class ExtString(x: String) {
    def pad (to: Int): String =
      if (x.size >= to) x
      else ("0" ++ x) pad to
  }

  implicit class ExtInt(x: Int) {
    def showTo (digits: Int): String =
      x.toString.pad(digits)
  }
  
  def main (args: Array[String]): Unit = {
    val input = readInput()
    val output = run(input)
    println(s"Problem 1: ${output(0)}")

    val (noun, verb) = calculateInputFor(input, 19690720)
    println(s"Problem 2: ${noun.showTo(2)}${verb.showTo(2)}")
  }
}
