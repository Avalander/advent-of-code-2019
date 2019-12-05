package advent

import org.scalatest._

import IntComputer._

class IntComputerTest extends FunSuite {
  val cases = List(
    (Vector(3,0))
  )

  test("Input should work") {
    val program = Vector(3,3,99,0)
    val inputs = List(15)
    val (result, _) = runExtended(program, inputs = inputs)
    assert(result == Vector(3,3,99,15))
  }

  test("Output should work") {
    val program = Vector(4,3,99,127)
    val (_, result) = runExtended(program)
    assert(result == List(127))
  }

  test("Should evaluate input and addition") {
    val program = Vector(
      3,11,
      1,11,6,6,
      1100,1,1,11,
      99,
      0,0)
    val inputs = List(1)
    val (result, _) = runExtended(program, inputs = inputs)
    assert(result == Vector(3,11,1,11,6,6,1101,1,1,11,99,2,0))
  }

  test("Addition") {
    val cases = List(
      (Vector(1,0,1,1,
              99), Vector(1,1,1,1,99)),
      (Vector(101, 5, 6, 6,
              99, 40, 50),
       Vector(101, 5, 6, 6,
              99, 40, 55)),
      (Vector(1001, 5, 6, 6,
              99, 40, 50),
       Vector(1001, 5, 6, 6,
              99, 40, 46)),
      (Vector(1101, 5, 6, 6,
              99, 40, 50),
       Vector(1101, 5, 6, 6,
              99, 40, 11))
    )

    cases foreach {
      case(input, output) =>
        assert(runExtended(input)._1 == output)
    }
  }

  test("Product") {
    val cases = List(
      (Vector(2,5,6,6,
              99, 40, 50),
       Vector(2,5,6,6,
              99, 40, 2000)),
      (Vector(102, 5, 6, 6,
              99, 40, 50),
       Vector(102, 5, 6, 6,
              99, 40, 250)),
      (Vector(1002, 5, 6, 6,
              99, 40, 50),
       Vector(1002, 5, 6, 6,
              99, 40, 240)),
      (Vector(1102, 5, 6, 6,
              99, 40, 50),
       Vector(1102, 5, 6, 6,
              99, 40, 30))
    )

    cases foreach {
      case(input, output) =>
        assert(runExtended(input)._1 == output)
    }
  }
}
