package advent

import scala.annotation.tailrec

object IntComputer {
  type Program = Vector[Int]
  type Outputs = List[Int]

  case class Instruction(code: Int, args: List[Int])

  def runExtended (
    program: Program,
    index: Int = 0,
    inputs: List[Int] = Nil,
    outputs: List[Int] = List()): (Program, Outputs) = {
    val code = program(index)
    val instruction = parseInstruction(code, program, index)
    instruction match {
      case Instruction(99, _) => (program, outputs)
      // addition
      case Instruction(1, List(a, b, _)) => {
        val valueA = getValue(index + 1, a, program)
        val valueB = getValue(index + 2, b, program)
        val dest = program(index + 3)
        val value = valueA + valueB
        val result = program.patch(dest, List(value), 1)
        runExtended(result, index + 4, inputs, outputs)
      }
      // product
      case Instruction(2, List(a, b, _)) => {
        val valueA = getValue(index + 1, a, program)
        val valueB = getValue(index + 2, b, program)
        val dest = program(index + 3)
        val value = valueA * valueB
        val result = program.patch(dest, List(value), 1)
        runExtended(result, index + 4, inputs, outputs)
      }
      // input
      case Instruction(3, _) => {
        val (input :: restInputs) = inputs
        val dest = program(index + 1)
        val result = program.patch(dest, List(input), 1)
        runExtended(result, index + 2, restInputs, outputs)
      }
      // output
      case Instruction(4, _) => {
        val output = program(program(index + 1))
        runExtended(program, index + 2, inputs, outputs :+ output)
      }
      // jump-if-true
      case Instruction(5, List(a, b)) => {
        val value = getValue(index + 1, a, program)
        val nextIndex = getValue(index + 2, b, program)
        if (value != 0) runExtended(program, nextIndex, inputs, outputs)
        else runExtended(program, index + 3, inputs, outputs)
      }
      // jump-if-false
      case Instruction(6, List(a, b)) => {
        val value = getValue(index + 1, a, program)
        val nextIndex = getValue(index + 2, b, program)
        if (value == 0) runExtended(program, nextIndex, inputs, outputs)
        else runExtended(program, index + 3, inputs, outputs)
      }
      // less-than
      case Instruction(7, List(a, b, _)) => {
        val valueA = getValue(index + 1, a, program)
        val valueB = getValue(index + 2, b, program)
        val dest = program(index + 3)
        val value =
          if (valueA < valueB) 1
          else 0
        val result = program.patch(dest, List(value), 1)
        runExtended(result, index + 4, inputs, outputs)
      }
      // equal
      case Instruction(8, List(a, b, _)) => {
        val valueA = getValue(index + 1, a, program)
        val valueB = getValue(index + 2, b, program)
        val dest = program(index + 3)
        val value =
          if (valueA == valueB) 1
          else 0
        val result = program.patch(dest, List(value), 1)
        runExtended(result, index + 4, inputs, outputs)
      }
    }
  }

  private def getValue (index: Int, mode: Int, program: Program): Int =
    mode match {
      case 0 => program(program(index))
      case 1 => program(index)
      case _ => throw new MatchError(s"No param mode $mode")
    }

  private def parseInstruction (code: Int, program: Program, index: Int) = {
    val strCode = f"$code%05d"
    val (a :: b :: c :: op) = strCode.toList
    val opCode = op.mkString.toInt
    var argsLength = argsForInstruction(opCode, index)
    val args = List(a, b, c).reverse take (argsLength) map (_.toString.toInt)
    Instruction(opCode, args)
  }

  private def argsForInstruction (code: Int, i: Int): Int =
    code match {
      case 1 => 3
      case 2 => 3
      case 3 => 1
      case 4 => 1
      case 5 => 2
      case 6 => 2
      case 7 => 3
      case 8 => 3
      case 99 => 0
      case x => throw new MatchError(s"Code $code at index $i")
    }
}
