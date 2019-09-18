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
  private enum Size(val out: String):
    case B extends Size("b")
    case L extends Size("l")

  /** This enum holds all the possible registers */
  private enum Reg:
    case RAX
    
    def sized(size: Size): Register = Register(this, size)

  /** This represents a reference to an actual register.
   *
   *  We can address a different section of a register based
   *  on the size of data we're working with.
   *
   *  @param name the name of this register
   *  @param size the size we're using this register with
   */
  private case class Register(name: Reg, size: Size):
    def sized(newsize: Size): Register = Register(name, newsize)

    def out: String = (name, size) match
      case (Reg.RAX, Size.B) => "%al"
      case (Reg.RAX, Size.L) => "%eax"
  
  /** This represents an argument to a given instruction */
  private enum Arg:
    /** We're using a value immediately. e.g. `$34` */
    case I(int: Int)
    /** We're using the direct value of a register, e.g. `%rax` */
    case R(reg: Register)

    def out: String = this match
      case I(int) => "$" + int.toString
      case R(reg) => reg.out

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
  def generate(program: IntMainReturn): Unit = generate(program)
    writeln("\t.globl\tmain")
    label("main")
    expr(program.expr)
    ret()

  private def expr(theExpr: PrimaryExpr): Register = theExpr match
    case Expr.Litteral(int) => litteral(int)
    case Expr.Not(theExpr) =>
      val regl = expr(theExpr)
      val regb = regl.sized(Size.B)
      // If int == 0, set first byte of int to 1, else 0
      test(Size.L, Arg.R(regl), Arg.R(regl))
      sete(Arg.R(regb))
      // Upgrade byte to full integer
      movz(Size.B, Size.L, Arg.R(regb), Arg.R(regl))
      regl
    case Expr.BitNot(theExpr) =>
      val regl = expr(theExpr)
      not(Size.L, Arg.R(regl))
      regl
    case Expr.Negate(theExpr) =>
      val regl = expr(theExpr)
      neg(Size.L, Arg.R(regl))
      regl

  private def litteral(int: Int): Register =
    val regl = Register(Reg.RAX, Size.L)
    mov(Size.L, Arg.I(int), Arg.R(regl))
    regl
  
  private def write(string: String): Unit =
    out.write(string.getBytes("UTF8"))
  
  private def writeln(string: String): Unit =
    write(string)
    write("\n")

  private def label(label: String): Unit =
    write(label)
    writeln(":")
  
  private def instruction(name: String, size: Size, source: Arg, dest: Arg): Unit =
    writeln(s"\t${name}${size.out}\t${source.out}, ${dest.out}")

  private def mov(size: Size, source: Arg, dest: Arg): Unit =
    instruction("mov", size, source, dest)
  
  private def movz(sourceSize: Size, destSize: Size, source: Arg, dest: Arg): Unit =
    writeln(s"\tmovz${sourceSize.out}${destSize.out}\t${source.out}, ${dest.out}")
  
  private def neg(size: Size, arg: Arg): Unit =
    writeln(s"\tneg${size.out}\t${arg.out}")

  private def not(size: Size, arg: Arg): Unit =
    writeln(s"\tnot${size.out}\t${arg.out}")

  private def sete(dest: Arg): Unit =
    writeln(s"\tsete\t${dest.out}")
  
  private def test(size: Size, source: Arg, dest: Arg): Unit =
    instruction("test", size, source, dest)

  private def ret(): Unit =
    writeln("\tret")
