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

  test("jump-if-true") {
    val input = Vector(3,3,1105,-1,9,1101,0,0,12,4,12,99,1)
    val result = runExtended(input, inputs = List(1))
    assert(result._1 == Vector(3,3,1105,1,9,1101,0,0,12,4,12,99,1))
    assert(result._2 == List(1))

    val result2 = runExtended(input, inputs = List(0))
    assert(result2._1 == Vector(3,3,1105,0,9,1101,0,0,12,4,12,99,0))
    assert(result2._2 == List(0))
  }

  test("equal") {
    val program = Vector(3,9,8,9,10,9,4,9,99,-1,8)
    val input1 = List(8)
    val result1 = runExtended(program, inputs = input1)
    assert(result1._2 == List(1))

    val input2 = List(7)
    val result2 = runExtended(program, inputs = input2)
    assert(result2._2 == List(0))
  }

  test("equal immediate mode") {
    val program = Vector(3,3,1108,-1,8,3,4,3,99)
    val input1 = List(8)
    val result1 = runExtended(program, inputs = input1)
    assert(result1._2 == List(1))

    val input2 = List(7)
    val result2 = runExtended(program, inputs = input2)
    assert(result2._2 == List(0))
  }

  test("less-than") {
    val program = Vector(3,9,7,9,10,9,4,9,99,-1,8)
    val input1 = List(7)
    val result1 = runExtended(program, inputs = input1)
    assert(result1._2 == List(1))

    val input2 = List(8)
    val result2 = runExtended(program, inputs = input2)
    assert(result2._2 == List(0))

    val input3 = List(9)
    val result3 = runExtended(program, inputs = input3)
    assert(result3._2 == List(0))
  }

  test("less-than immediate mode") {
    val program = Vector(3,3,1107,-1,8,3,4,3,99)
    val input1 = List(7)
    val result1 = runExtended(program, inputs = input1)
    assert(result1._2 == List(1))

    val input2 = List(8)
    val result2 = runExtended(program, inputs = input2)
    assert(result2._2 == List(0))

    val input3 = List(9)
    val result3 = runExtended(program, inputs = input3)
    assert(result3._2 == List(0))
  }

  test("jump-if-false") {
    val input = Vector(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)
    val result = runExtended(input, inputs = List(1))
    // assert(result._1 == Vector(3,12,6,12,15,1,13,14,13,4,13,99,1,1,1,9))
    assert(result._2 == List(1))

    val result2 = runExtended(input, inputs = List(0))
    // assert(result2._1 == Vector(3,3,1105,0,9,1101,0,0,12,4,12,99,0))
    assert(result2._2 == List(0))
  }
}
