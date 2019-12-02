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
      patched = program.patch(1, List(noun), 1).patch(2, List(verb), 1)
      output = run(patched)
      if output(0) == target
    } yield (noun, verb)
    println(result)
    result(0)
  }
  
  def main (args: Array[String]): Unit = {
    val input = Source.fromResource("day-02.txt").getLines()
    val program = (for {
      line <- input
      value <- line.split(',')
    } yield value.toInt).toVector
      .patch(1, List(12), 1)
      .patch(2, List(2), 1)
    
    val output = run(program)
    println(s"Problem 1: ${output(0)}")

    val result2 = calculateInputFor(program, 19690720)
    println(s"Problem 2: $result2")
  }
}
