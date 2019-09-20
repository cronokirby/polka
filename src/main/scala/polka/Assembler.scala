package polka

import Syntax._

import java.io.OutputStream
import scala.collection.mutable.{Map, Stack}

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
    private def binaryOp(op: String, source: Int | Reg): Outputting[Unit] = source match
      case i: Int => binaryOp(op, "$" + i)
      case r: Reg => binaryOp(op, r.sized(size).asm)

    private def unaryOp(op: String): Outputting[Unit] =
      writeln(s"\t${op}${size.asm}\t$asm")

    def add(source: Int | Reg): Outputting[Unit] = binaryOp("add", source)

    def mul(source: Int | Reg): Outputting[Unit] = binaryOp("imul", source)

    def mov(source: Int | Reg): Outputting[Unit] = binaryOp("mov", source)

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
  def generate(program: IR): Unit =
    writeln("\t.globl\tmain")
    writeln("main:")
    statements(program.statements)

  private def statements(stmts: Vector[IR.Statement]): Unit =
    import IR.Operand._
    val freeRegisters = Stack(Reg.RAX, Reg.RBX, Reg.RCX)
    val owners = Map[Int, Reg]()

    def applyOp(op: IR.BinOp, reg: Reg, right: Int | Reg): Unit =
      val regl = reg.sized(Size.L)
      op match
      case IR.BinOp.Add => regl.add(right)
      case IR.BinOp.Times => regl.mul(right)

    for s <- stmts do s match
      case IR.Statement.Initialize(name, value) =>
        val reg = freeRegisters.pop()
        owners += name.index -> reg
        reg.sized(Size.L).mov(value)
      case IR.Statement.ApplyUnary(to, op, arg) =>
        val reg = owners(arg.index)
        owners += to.index -> reg
        op match
        case IR.UnaryOp.BitNot => reg.sized(Size.L).not()
        case IR.UnaryOp.Negate => reg.sized(Size.L).neg()
        case IR.UnaryOp.Not =>
          val regl = reg.sized(Size.L)
          regl.test(reg)
          regl.sete()
          regl.movz(reg.sized(Size.B))
      case IR.Statement.ApplyBin(to, op, OnInt(l), OnInt(r)) =>
        val reg = freeRegisters.pop()
        owners += to.index -> reg
        reg.sized(Size.L).mov(l)
        applyOp(op, reg, r)
      case IR.Statement.ApplyBin(to, op, OnInt(int), OnName(source)) =>
        val reg = owners(source.index)
        owners += to.index -> reg
        applyOp(op, reg, int)
      case IR.Statement.ApplyBin(to, op, OnName(source), OnInt(int)) =>
        val reg = owners(source.index)
        owners += to.index -> reg
        applyOp(op, reg, int)
      case IR.Statement.ApplyBin(to, op, OnName(left), OnName(right)) =>
        val owned = owners(left.index)
        val freed = owners(right.index)
        owners += to.index -> owned
        freeRegisters.push(freed)
        applyOp(op, owned, freed)
      case IR.Statement.Return(name) =>
        val reg = owners(name.index)
        if reg != Reg.RAX then Reg.RAX.sized(Size.L).mov(reg)
        writeln("\tret")
