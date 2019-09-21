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
    def doesMath: Boolean = false

  private case class Constant(value: Int) extends AsmArg:
    def asm(size: Size) = "$" + value

  /** This enum holds all the possible registers */
  private enum Reg extends AsmArg:
    case RAX
    case RBX
    case RCX
    case RSP
    case R10

    override def doesMath = true

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
      case (RSP, Size.B) => "%spl"
      case (RSP, Size.L) => "%esp"
      case (RSP, Size.Q) => "%rsp"
      case (R10, Size.B) => "%r10b"
      case (R10, Size.L) => "%r10d"
      case (R10, Size.Q) => "%r10"

  private case class Shifted(reg: Reg, by: Int) extends AsmArg:
    def asm(size: Size) = s"$by(${reg.asm(Size.Q)})"

  private class StatementCtx(private val free: Stack[AsmArg], val scratch: Reg, val epilogue: Outputting[Unit]):
    private val owners = Map[Int, AsmArg]()

    def getReg(name: IR.Name): AsmArg = owners(name.index)

    def newReg(name: IR.Name): AsmArg =
      val reg = free.pop()
      owners += name.index -> reg
      reg

    def reuseReg(from: IR.Name, to: IR.Name): AsmArg =
      val reg = owners(from.index)
      owners += to.index -> reg
      reg

    def freeReg(from: IR.Name): AsmArg =
      val freed = owners(from.index)
      free.push(freed)
      freed

  private def binaryOp(op: String, size: Size, source: AsmArg, dest: AsmArg): Outputting[Unit] =
    writeln(s"\t${op}${size.asm}\t${source.asm(size)}, ${dest.asm(size)}")

  private def unaryOp(op: String, size: Size, dest: AsmArg): Outputting[Unit] =
    writeln(s"\t${op}${size.asm}\t${dest.asm(size)}")

  private def add(size: Size, source: AsmArg, dest: AsmArg): Outputting[Unit] =
    binaryOp("add", size, source, dest)

  private def sub(size: Size, source: AsmArg, dest: AsmArg): Outputting[Unit] =
    binaryOp("sub", size, source, dest)

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
  private def test(size: Size, source: AsmArg, dest: AsmArg): Outputting[Unit] =
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
    val counts = stmts.scanLeft(0):
      (count, s) => s match
      case IR.Statement.Initialize(_, _) => count + 1
      case IR.Statement.ApplyBin(_, _, left, right) =>
        val addLeft = if left.isName then 1 else 0
        val addRight = if right.isName then 1 else 0
        count + 1 - addLeft - addRight
      case _ => count
    val maxCount = counts.reduce(_ max _)
    val freeRegisters = Stack[AsmArg](Reg.RAX, Reg.RBX, Reg.RCX)
    val overflow = maxCount - freeRegisters.length
    for stackIndex <- 0 to overflow do
      freeRegisters.append(Shifted(Reg.RSP, stackIndex * 8))
    sub(Size.Q, Constant(overflow * 8), Reg.RSP)
    val ctx = StatementCtx(freeRegisters, Reg.R10, add(Size.Q, Constant(overflow * 8), Reg.RSP))
    stmts.foreach(statement(ctx, _))

  private def statement(ctx: StatementCtx, stmt: IR.Statement): Unit =
    import IR.Operand._

    def applyOp(op: IR.BinOp, source: AsmArg, dest: AsmArg): Unit = op match
      case IR.BinOp.Add =>
        add(Size.L, source, dest)
      case IR.BinOp.Times =>
        if dest.doesMath then
          mul(Size.L, source, dest)
        else
          mov(Size.Q, dest, ctx.scratch)
          mul(Size.L, source, ctx.scratch)
          mov(Size.Q, ctx.scratch, dest)

    stmt match
    case IR.Statement.Initialize(name, value) =>
      val reg = ctx.newReg(name)
      mov(Size.L, Constant(value), reg)
    case IR.Statement.ApplyUnary(to, op, arg) =>
      val reg = ctx.reuseReg(arg, to)
      op match
      case IR.UnaryOp.BitNot => not(Size.L, reg)
      case IR.UnaryOp.Negate => neg(Size.L, reg)
      case IR.UnaryOp.Not =>
        test(Size.L, reg, reg)
        sete(reg)
        movz(Size.B, Size.L, reg, reg)
    case IR.Statement.ApplyBin(to, op, OnInt(l), OnInt(r)) =>
      val reg = ctx.newReg(to)
      mov(Size.L, Constant(l), reg)
      applyOp(op, Constant(r), reg)
    case IR.Statement.ApplyBin(to, op, OnInt(int), OnName(source)) =>
      val reg = ctx.reuseReg(source, to)
      applyOp(op, Constant(int), reg)
    case IR.Statement.ApplyBin(to, op, OnName(source), OnInt(int)) =>
      val reg = ctx.reuseReg(source, to)
      applyOp(op, Constant(int), reg)
    case IR.Statement.ApplyBin(to, op, OnName(left), OnName(right)) =>
      val owned = ctx.reuseReg(left, to)
      val freed = ctx.freeReg(right)
      applyOp(op, freed, owned)
    case IR.Statement.Return(name) =>
      val reg = ctx.getReg(name)
      if reg != Reg.RAX then mov(Size.L, reg, Reg.RAX)
      ctx.epilogue
      writeln("\tret")
