package advent

import scala.io.Source
import scala.annotation.tailrec

object Fuel {
  def amountRequired (mass: Int): Int =
    mass / 3 - 2 
  
  def fuelAdjusted (mass: Int): Int =
    withFuel(mass)

  @tailrec
  private def withFuel (mass: Int, fuel: List[Int] = List()): Int = {
    fuel match {
      case Nil       => withFuel(mass, List(amountRequired(mass)))
      case (x :: xs) => {
        val fuelForLast = amountRequired(x)
        if (fuelForLast <= 0) fuel.sum
        else withFuel(mass, fuelForLast :: fuel)
      }
    }
  }
  
  def main (args: Array[String]): Unit = {
    val input = Source.fromResource("day-01.txt").getLines().toList
    val result = (for {
      line <- input
      value = line.toInt
    } yield amountRequired(value)).sum
    println(s"Problem 1: $result")

    val result2 = (for {
      line <- input
      value = fuelAdjusted(line.toInt)
    } yield value).sum
    println(s"Problem 2: $result2")
  }
}
