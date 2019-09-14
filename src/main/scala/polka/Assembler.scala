package polka

import Syntax._

import java.io.OutputStream

object Assembler:

  enum Size(val output: String):
    case L extends Size("l")
  
  enum Arg:
    case Litteral(int: Int)
    // This should be replaced with a hardcoded register at some point
    case EAX

    def output: String = this match
      case Litteral(int) => "$" + int.toString
      case EAX => "%eax"

class Assembler(private val out: OutputStream):
  import Assembler._

  def output(program: IntMainReturn): Unit =
    writeln("\t.globl\tmain")
    label("main")
    program.expr match
    case Expr.IntLitteral(int) => mov(Size.L, Arg.Litteral(int), Arg.EAX)
    ret()
  
  private def write(string: String): Unit =
    out.write(string.getBytes("UTF8"))
  
  private def writeln(string: String): Unit =
    write(string)
    write("\n")

  private def label(label: String): Unit =
    write(label)
    writeln(":")

  private def mov(size: Size, source: Arg, dest: Arg): Unit =
    writeln(s"\tmov${size.output}\t${source.output}, ${dest.output}")

  private def ret(): Unit =
    writeln("\tret")
