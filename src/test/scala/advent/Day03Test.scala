package advent

import org.scalatest._

import Day03._

class Day03Test extends FunSuite {
  val cases = List(
    (
      List("R75","D30","R83","U83","L12","D49","R71","U7","L72"),
      List("U62","R66","U55","R34","D71","R55","D58","R83"),
      159
    ),
    (
      List("R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"),
      List("U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"),
      135
    )
  )

  cases foreach {
    case (a, b, expected) =>
      test(s"closestIntersection($a, $b) returns $expected") {
        assert(closestIntersection(a, b) == expected)
      }
  }

}
