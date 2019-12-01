package advent

import org.scalatest._

import Fuel.amountRequired

class Day01Test extends FunSuite {
  val cases = List(
    (12, 2),
    (14, 2),
    (1969, 654),
    (100756, 33583)
  )

  cases foreach {
    case (mass, fuel) =>
      test(s"amountRequired($mass) returns $fuel") {
        assert(amountRequired(mass) == fuel)
      }
  }
}

/*

    For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2.
    For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel required is also 2.
    For a mass of 1969, the fuel required is 654.
    For a mass of 100756, the fuel required is 33583.
*/
