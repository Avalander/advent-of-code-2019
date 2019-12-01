package advent

import scala.io.Source

object Fuel {
  def amountRequired (mass: Int): Int =
    mass / 3 - 2 
  
  def main (args: Array[String]): Unit = {
    val input = Source.fromResource("day-01.txt").getLines()
    val result = (for {
      line <- input
      value = line.toInt
    } yield amountRequired(value)).sum
    println(result)
  }
}
