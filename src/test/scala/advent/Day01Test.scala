package advent

import org.scalatest._

import Fuel._

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

  val cases2 = List(
    (12, 2),
    (14, 2),
    (1969, 966),
    (100756, 50346)
  )

  cases2 foreach {
    case (mass, fuel) =>
      test(s"fuelAdjusted($mass) returns $fuel") {
        assert(fuelAdjusted(mass) == fuel)
      }
  }
}
