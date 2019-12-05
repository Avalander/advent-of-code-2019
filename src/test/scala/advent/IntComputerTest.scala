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
    val program = Vector(3,11,1,11,6,6,1100,1,1,12,99,0,0)
    val inputs = List(1)
    val (result, _) = runExtended(program, inputs = inputs)
    assert(result == Vector(3,11,1,11,6,6,1101,1,1,12,99,1,4))
    
  }
}
