package polka

import Syntax._

import java.io.OutputStream

object Assembler:

  /** Represents the size used by an instruction.
   *
   *  Assembly instructions come in different variants. For example,
   *  the `mov` instruction can be used with `movq`, or `movl` etc.
   *  Each of these is operating on a differently sized datatype
   */
  enum Size(val output: String):
    case L extends Size("l")

  /** This enum holds all the possible registers */
  enum Reg:
    case RAX

  /** This represents a reference to an actual register.
   *
   *  We can address a different section of a register based
   *  on the size of data we're working with.
   *
   *  @param name the name of this register
   *  @param size the size we're using this register with
   */
  case class Register(name: Reg, size: Size):
    def output: String = (name, size) match
      case (Reg.RAX, Size.L) => "%eax"
  
  /** This represents an argument to a given instruction */
  enum Arg:
    /** We're using a value immediately. e.g. `$34` */
    case Immediate(int: Int)
    /** We're using the direct value of a register, e.g. `%rax` */
    case Direct(reg: Register)

    def output: String = this match
      case Immediate(int) => "$" + int.toString
      case Direct(reg) => reg.output

/** A code generator, hooked into an output stream.
 *
 *  @param out the output stream this writes to
 */
class Assembler(private val out: OutputStream):
  import Assembler._

  /** Generate the assembly for a program.
   *
   *  @param program the AST for our C program.
   */
  def generate(program: IntMainReturn): Unit =
    writeln("\t.globl\tmain")
    label("main")
    program.expr match
    case Expr.IntLitteral(int) =>
      val register = Register(Reg.RAX, Size.L)
      mov(Size.L, Arg.Immediate(int), Arg.Direct(register))
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
