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

  private trait AsmArg:
    def asm(size: Size): String

  private case class Constant(value: Int) extends AsmArg:
    def asm(size: Size) = "$" + value

  /** This enum holds all the possible registers */
  private enum Reg extends AsmArg:
    case RAX
    case RBX
    case RCX

    def asm(size: Size) = (this, size) match
      case (RAX, Size.B) => "%al"
      case (RAX, Size.L) => "%eax"
      case (RAX, Size.Q) => "%rax"
      case (RBX, Size.B) => "%bl"
      case (RBX, Size.L) => "%ebx"
      case (RBX, Size.Q) => "%rbx"
      case (RCX, Size.B) => "%cl"
      case (RCX, Size.L) => "%ecx"
      case (RCX, Size.Q) => "%rcx"

  private def binaryOp(op: String, size: Size, source: AsmArg, dest: AsmArg): Outputting[Unit] =
    writeln(s"\t${op}${size.asm}\t${source.asm(size)}, ${dest.asm(size)}")

  private def unaryOp(op: String, size: Size, dest: AsmArg): Outputting[Unit] =
    writeln(s"\t${op}${size.asm}\t${dest.asm(size)}")

  private def add(size: Size, source: AsmArg, dest: AsmArg): Outputting[Unit] =
    binaryOp("add", size, source, dest)

  private def mul(size: Size, source: AsmArg, dest: AsmArg): Outputting[Unit] =
    binaryOp("imul", size, source, dest)

  private def mov(size: Size, source: AsmArg, dest: AsmArg): Outputting[Unit] =
    binaryOp("mov", size, source, dest)

  private def movz(sourceSize: Size, destSize: Size, source: AsmArg, dest: AsmArg): Outputting[Unit] =
    writeln(s"\tmovz$sourceSize$destSize\t${source.asm(sourceSize)}, ${dest.asm(destSize)}")

  private def neg(size: Size, dest: AsmArg): Outputting[Unit] =
    unaryOp("neg", size, dest)

  private def not(size: Size, dest: AsmArg): Outputting[Unit] =
    unaryOp("not", size, dest)

  private def pop(dest: AsmArg): Outputting[Unit] =
    unaryOp("pop", Size.Q, dest)

  private def push(dest: AsmArg): Outputting[Unit] =
    unaryOp("push", Size.Q, dest)

  // This operator only works with bytes
  private def sete(dest: AsmArg): Outputting[Unit] =
    writeln(s"\tsete\t${dest.asm(Size.B)}")

  // This operator's destination is always a byte
  private def test(size: Size, source: Reg, dest: Reg): Outputting[Unit] =
    binaryOp("test", size, source, dest)

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

    def applyOp(op: IR.BinOp, source: AsmArg, dest: Reg): Unit = op match
      case IR.BinOp.Add => add(Size.L, source, dest)
      case IR.BinOp.Times => mul(Size.L, source, dest)

    for s <- stmts do s match
      case IR.Statement.Initialize(name, value) =>
        val reg = freeRegisters.pop()
        owners += name.index -> reg
        mov(Size.L, Constant(value), reg)
      case IR.Statement.ApplyUnary(to, op, arg) =>
        val reg = owners(arg.index)
        owners += to.index -> reg
        op match
        case IR.UnaryOp.BitNot => not(Size.L, reg)
        case IR.UnaryOp.Negate => neg(Size.L, reg)
        case IR.UnaryOp.Not =>
          test(Size.L, reg, reg)
          sete(reg)
          movz(Size.B, Size.L, reg, reg)
      case IR.Statement.ApplyBin(to, op, OnInt(l), OnInt(r)) =>
        val reg = freeRegisters.pop()
        owners += to.index -> reg
        mov(Size.L, Constant(l), reg)
        applyOp(op, Constant(r), reg)
      case IR.Statement.ApplyBin(to, op, OnInt(int), OnName(source)) =>
        val reg = owners(source.index)
        owners += to.index -> reg
        applyOp(op, Constant(int), reg)
      case IR.Statement.ApplyBin(to, op, OnName(source), OnInt(int)) =>
        val reg = owners(source.index)
        owners += to.index -> reg
        applyOp(op, Constant(int), reg)
      case IR.Statement.ApplyBin(to, op, OnName(left), OnName(right)) =>
        val owned = owners(left.index)
        val freed = owners(right.index)
        owners += to.index -> owned
        freeRegisters.push(freed)
        applyOp(op, freed, owned)
      case IR.Statement.Return(name) =>
        val reg = owners(name.index)
        if reg != Reg.RAX then mov(Size.L, reg, Reg.RAX)
        writeln("\tret")
