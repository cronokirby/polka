package polka

import Syntax._

import java.io.OutputStream

object Assembler:
  private type Outputting[T] = given OutputStream => T

  private def writeln(line: String): Outputting[Unit] =
    val encoding = "UTF8"
    val out = the[OutputStream]
    out.write(line.getBytes(encoding))
    out.write("\n".getBytes(encoding))

  /** Represents the size used by an instruction.
   *
   *  Assembly instructions come in different variants. For example,
   *  the `mov` instruction can be used with `movq`, or `movl` etc.
   *  Each of these is operating on a differently sized datatype
   */
  private enum Size(val asm: String):
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

    def asm: String = (name, size) match
      case (Reg.RAX, Size.B) => "%al"
      case (Reg.RAX, Size.L) => "%eax"
      case (Reg.RAX, Size.Q) => "%rax"
      case (Reg.RBX, Size.B) => "%bl"
      case (Reg.RBX, Size.L) => "%ebx"
      case (Reg.RBX, Size.Q) => "%rbx"
      case (Reg.RCX, Size.B) => "%cl"
      case (Reg.RCX, Size.L) => "%ecx"
      case (Reg.RCX, Size.Q) => "%rcx"

    private def binaryOp(op: String, source: String): Outputting[Unit] =
      writeln(s"\t${op}${size.asm}\t$source, $asm")
    private def binaryOp(op: String, source: Int): Outputting[Unit] =
      binaryOp(op, "$" + source)
    private def binaryOp(op: String, source: Reg): Outputting[Unit] =
      binaryOp(op, source.sized(size).asm)
    
    private def unaryOp(op: String): Outputting[Unit] =
      writeln(s"\t${op}${size.asm}\t$asm")

    def add(source: Int): Outputting[Unit] = binaryOp("add", source)
    def add(source: Reg): Outputting[Unit] = binaryOp("add", source)
    
    def mul(source: Int): Outputting[Unit] = binaryOp("imul", source)
    def mul(source: Reg): Outputting[Unit] = binaryOp("imul", source)

    def mov(source: Int): Outputting[Unit] = binaryOp("mov", source)
    def mov(source: Reg): Outputting[Unit] = binaryOp("mov", source)
    
    def movz(source: Register): Outputting[Unit] =
      writeln(s"\tmovz${source.size.asm}${size.asm}\t${source.asm}, $asm")
    
    def neg(): Outputting[Unit] = unaryOp("neg")

    def not(): Outputting[Unit] = unaryOp("not")

    def pop(): Outputting[Unit] = unaryOp("pop")

    def push(): Outputting[Unit] = unaryOp("push")

    // This operator only works with bytes
    def sete(): Outputting[Unit] = writeln(s"\tsete\t${sized(Size.B).asm}")

    // This operator's destination is always a byte
    def test(source: Reg): Outputting[Unit] = sized(Size.B).binaryOp("test", source)

/** A code generator, hooked into an output stream.
 *
 *  @param out the output stream this writes to
 */
class Assembler(private val out: OutputStream):
  import Assembler._

  given as OutputStream = out

  /** Generate the assembly for a program.
   *
   *  @param program the AST for our C program.
   */
  def generate(program: IntMainReturn): Unit =
    writeln("\t.globl\tmain")
    writeln("main:")
    add(program.expr)
    Reg.RAX.sized(Size.Q).pop()
    writeln("\tret")
  
  def add(expr: Add): Unit =
    val owned = Reg.RAX.sized(Size.L)
    val scratch = Reg.RCX
    owned.mov(0)
    expr.exprs.foreach:
      m =>
        multiply(m)
        scratch.sized(Size.Q).pop()
        owned.add(scratch)
    // We need this if this expression is in ()
    owned.sized(Size.Q).push()

  def multiply(expr: Multiply): Unit =
    val owned = Reg.RBX.sized(Size.L)
    val scratch = Reg.RCX
    owned.mov(1)
    expr.exprs.foreach:
      e =>
        primExpr(e)
        scratch.sized(Size.Q).pop()
        owned.mul(scratch)
    // We need this if this expression is in ()
    owned.sized(Size.Q).push()

  private def primExpr(theExpr: PrimaryExpr): Unit = theExpr match
    case PrimaryExpr.Litteral(int) => litteral(int)
    case PrimaryExpr.Not(theExpr) =>
      primExpr(theExpr)
      val reg = Reg.RAX
      val regl = reg.sized(Size.L)
      reg.sized(Size.Q).pop()
      // If int == 0, set first byte of int to 1, else 0
      regl.test(reg)
      regl.sete()
      // Upgrade byte to full integer
      reg.sized(Size.B).movz(regl)
      reg.sized(Size.Q).push()
    case PrimaryExpr.BitNot(theExpr) =>
      primExpr(theExpr)
      val reg = Reg.RAX.sized(Size.Q)
      reg.pop()
      reg.sized(Size.L).not()
      reg.push()
    case PrimaryExpr.Negate(theExpr) =>
      primExpr(theExpr)
      val reg = Reg.RAX.sized(Size.Q)
      reg.pop()
      reg.sized(Size.L).neg()
      reg.push()
    case PrimaryExpr.Parens(a) => add(a)

  private def litteral(int: Int): Unit =
    val arg = "$" + int
    // NOTE: Abstract this?
    writeln(s"\tpushq\t$arg")
