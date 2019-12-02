package advent

import org.scalatest._

import Day02._

class Day02Test extends FunSuite {
  val cases = List(
    (Vector(1,0,0,0,99), Vector(2,0,0,0,99)),
    (Vector(2,3,0,3,99), Vector(2,3,0,6,99)),
    (Vector(2,4,4,5,99,0), Vector(2,4,4,5,99,9801)),
    (Vector(1,1,1,4,99,5,6,0,99), Vector(30,1,1,4,2,5,6,0,99))
  )

  cases foreach {
    case (p, expected) =>
      test(s"run($p) returns $expected") {
        assert(Day02.run(p) === expected)
      }
  }
}
