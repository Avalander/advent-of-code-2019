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
      case Instruction(1, List(a, b, dest)) => {
        val value = a + b
        println(s"$a, $b, $dest")
        val result = program.patch(dest, List(value), 1)
        runExtended(result, index + 4, inputs, outputs)
      }
      case Instruction(2, List(a, b, dest)) => {
        val value = a * b
        val result = program.patch(dest, List(value), 1)
        runExtended(result, index + 4, inputs, outputs)
      }
      case Instruction(3, (dest :: _)) => {
        val (input :: restInputs) = inputs
        val result = program.patch(dest, List(input), 1)
        runExtended(result, index + 2, restInputs, outputs)
      }
      case Instruction(4, (dest :: _)) => {
        val output = program(dest)
        runExtended(program, index + 2, inputs, outputs :+ output)
      }
    }
  }

  private def parseInstruction (code: Int, program: Program, index: Int) = {
    val strCode = 
      if (code < 10) defaultModes(code)
      else f"$code%05d"
    val (a :: b :: c :: op) = strCode.toList
    val opCode = op.mkString.toInt
    println(strCode)
    var argsLength = argsForInstruction(opCode, index)
    val args = List(a, b, c).reverse take (argsLength) map (_.toString.toInt) zip (1 to 3) map {
      case (0, i) => program(program(index + i))
      case (1, i) => program(index + i)
      case (x, _) => throw new MatchError(s"No param mode $x")
    }
    Instruction(opCode, args)
  }

  private def argsForInstruction (code: Int, i: Int): Int =
    code match {
      case 1 => 3
      case 2 => 3
      case 3 => 1
      case 4 => 1
      case 99 => 0
      case x => throw new MatchError(s"Code $code at index $i")
    }
  
  private def defaultModes (code: Int): String =
    code match {
      case 1 => "10001"
      case 2 => "10002"
      case 3 => "00103"
      case 4 => "00104"
    }
}
