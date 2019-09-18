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
    case Q extends Size("q")

  /** This enum holds all the possible registers */
  private enum Reg:
    case RAX
    case RBX
    case RCX
    
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
      case (Reg.RAX, Size.Q) => "%rax"
      case (Reg.RBX, Size.B) => "%bl"
      case (Reg.RBX, Size.L) => "%ebx"
      case (Reg.RBX, Size.Q) => "%rbx"
      case (Reg.RCX, Size.B) => "%cl"
      case (Reg.RCX, Size.L) => "%ecx"
      case (Reg.RCX, Size.Q) => "%rcx"
  
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
  def generate(program: IntMainReturn): Unit =
    writeln("\t.globl\tmain")
    label("main")
    add(program.expr)
    pop(Size.Q, Arg.R(Reg.RAX.sized(Size.Q)))
    ret()
  
  def add(expr: Add): Unit =
    val owned = Reg.RAX.sized(Size.L)
    val scratch = Reg.RCX.sized(Size.L)
    mov(Size.L, Arg.I(0), Arg.R(owned))
    expr.exprs.foreach:
      m =>
        multiply(m)
        pop(Size.Q, Arg.R(scratch.sized(Size.Q)))
        add(Size.L, Arg.R(scratch), Arg.R(owned))
    // We need this if this expression is in ()
    push(Size.Q, Arg.R(owned.sized(Size.Q)))

  def multiply(expr: Multiply): Unit =
    val owned = Reg.RBX.sized(Size.L)
    val scratch = Reg.RCX.sized(Size.L)
    mov(Size.L, Arg.I(1), Arg.R(owned))
    expr.exprs.foreach:
      e =>
        primExpr(e)
        pop(Size.Q, Arg.R(scratch.sized(Size.Q)))
        mul(Size.L, Arg.R(scratch), Arg.R(owned))
    // We need this if this expression is in ()
    push(Size.Q, Arg.R(owned.sized(Size.Q)))

  private def primExpr(theExpr: PrimaryExpr): Unit = theExpr match
    case PrimaryExpr.Litteral(int) => litteral(int)
    case PrimaryExpr.Not(theExpr) =>
      primExpr(theExpr)
      val regl = Reg.RAX.sized(Size.L)
      val regb = regl.sized(Size.B)
      pop(Size.Q, Arg.R(regl.sized(Size.Q)))
      // If int == 0, set first byte of int to 1, else 0
      test(Size.L, Arg.R(regl), Arg.R(regl))
      sete(Arg.R(regb))
      // Upgrade byte to full integer
      movz(Size.B, Size.L, Arg.R(regb), Arg.R(regl))
      push(Size.Q, Arg.R(regl.sized(Size.Q)))
    case PrimaryExpr.BitNot(theExpr) =>
      primExpr(theExpr)
      val regl = Reg.RAX.sized(Size.L)
      pop(Size.Q, Arg.R(regl.sized(Size.Q)))
      not(Size.L, Arg.R(regl))
      push(Size.Q, Arg.R(regl.sized(Size.Q)))
    case PrimaryExpr.Negate(theExpr) =>
      primExpr(theExpr)
      val regl = Reg.RAX.sized(Size.L)
      pop(Size.Q, Arg.R(regl.sized(Size.Q)))
      neg(Size.L, Arg.R(regl))
      push(Size.Q, Arg.R(regl.sized(Size.Q)))
    case PrimaryExpr.Parens(a) => add(a)

  private def litteral(int: Int): Unit =
    push(Size.Q, Arg.I(int))
  
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
  
  private def add(size: Size, source: Arg, dest: Arg): Unit =
    instruction("add", size, source, dest)
  
  private def mul(size: Size, source: Arg, dest: Arg): Unit =
    instruction("imul", size, source, dest)

  private def mov(size: Size, source: Arg, dest: Arg): Unit =
    instruction("mov", size, source, dest)
  
  private def movz(sourceSize: Size, destSize: Size, source: Arg, dest: Arg): Unit =
    writeln(s"\tmovz${sourceSize.out}${destSize.out}\t${source.out}, ${dest.out}")
  
  private def neg(size: Size, arg: Arg): Unit =
    writeln(s"\tneg${size.out}\t${arg.out}")

  private def not(size: Size, arg: Arg): Unit =
    writeln(s"\tnot${size.out}\t${arg.out}")
  
  private def pop(size: Size, arg: Arg): Unit =
    writeln(s"\tpop${size.out}\t${arg.out}")
  
  private def push(size: Size, arg: Arg): Unit =
    writeln(s"\tpush${size.out}\t${arg.out}")

  private def sete(dest: Arg): Unit =
    writeln(s"\tsete\t${dest.out}")
  
  private def test(size: Size, source: Arg, dest: Arg): Unit =
    instruction("test", size, source, dest)

  private def ret(): Unit =
    writeln("\tret")
