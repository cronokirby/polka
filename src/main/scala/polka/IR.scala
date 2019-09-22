package polka

import AST._

import java.util.StringJoiner
import scala.collection.mutable.Buffer

object IR:
  /** Represents a temporary variable in the IR.
   *
   *  There are some rules governing when these can be used:
   *  - They can only appear on the left side of a statement once
   *  - They can only appear on the right side of a statement once
   *
   * @param index the index of this name
   */
  case class Name(index: Int):
    def pprint: String = s"#$index"

  /** Represents a binary operation between two operands */
  enum BinOp:
    /** Add the two operands using `+` */
    case Add
    /** Add the two operands using `*` */
    case Times

    def pprint: String = this match
      case Add => "+"
      case Times => "*"

  object BinOp:
    def fromAST(op: AST.BinOp): BinOp = op match
      case AST.BinOp.Add => BinOp.Add
      case AST.BinOp.Times => BinOp.Times

  /** Represents a unary operation against a single operand */
  enum UnaryOp:
    /** The operator `!` */
    case Not
    /** The operator `~` */
    case BitNot
    /** The operator `-` */
    case Negate

    def pprint: String = this match
      case Not => "!"
      case BitNot => "~"
      case Negate => "-"

  object UnaryOp:
    def fromAST(op: AST.UnaryOp): UnaryOp = op match
      case AST.UnaryOp.Not => UnaryOp.Not
      case AST.UnaryOp.BitNot => UnaryOp.BitNot
      case AST.UnaryOp.Negate => UnaryOp.Negate

  enum Operand:
    case OnInt(value: Int)
    case OnName(name: Name)

    def isName: Boolean = this match
      case OnName(_) => true
      case OnInt(_) => false

    def pprint: String = this match
      case OnInt(int) => int.toString
      case OnName(name) => name.pprint

  /** Represents a TAC statement */
  enum Statement:
    /** Represents a unary operation on a given variable */
    case ApplyUnary(to: Name, op: UnaryOp, single: Name)
    /** Represents a binary operation between two operands */
    case ApplyBin(to: Name, op: BinOp, left: Operand, right: Operand)
    /** Initialize a given variable with a value */
    case Initialize(name: Name, as: Int)
    /** Return the value in a variable */
    case Return(value: Name)

    def pprint: String = this match
      case ApplyUnary(to, op, single) => s"${to.pprint} = ${op.pprint}${single.pprint}"
      case ApplyBin(to, op, l, r) => s"${to.pprint} = ${l.pprint} ${op.pprint} ${r.pprint}"
      case Initialize(name, as) => s"${name.pprint} = $as"
      case Return(value: Name) => s"ret ${value.pprint}"

  def from(program: IntMainReturn): IR = Generator().from(program)

  class Generator:
    val buf = Buffer[Statement]()
    var tempName = 0

    private def nextName(): Name =
      val name = Name(tempName)
      tempName += 1
      name

    private def gen(stmt: Statement): Unit = buf += stmt

    def from(program: IntMainReturn): IR =
      val name = expr(program.expr) match
        case Operand.OnInt(int) => createInt(int)
        case Operand.OnName(name) => name
      gen(Statement.Return(name))
      IR(buf.toVector)

    private def expr(e: Expr): Operand = e match
      case Expr.Litteral(int) => Operand.OnInt(int)
      case Expr.Binary(op, terms) => reduceOp(terms, BinOp.fromAST(op))
      case Expr.Unary(op, term) =>
        val name = createName(expr(term))
        val next = nextName()
        gen(Statement.ApplyUnary(next, UnaryOp.fromAST(op), name))
        Operand.OnName(next)

    private def reduceOp(exprs: Vector[Expr], op: BinOp): Operand =
      val root = expr(exprs.head)
      if exprs.length == 1 then
        root
      else
        var name = root match
          case Operand.OnInt(int) => createInt(int)
          case Operand.OnName(name) => name
        for e <- exprs.tail do
          val operand = expr(e)
          val oldName = Operand.OnName(name)
          name = nextName()
          gen(Statement.ApplyBin(name, op, oldName, operand))
        Operand.OnName(name)

    private def createInt(int: Int): Name =
      val name = nextName()
      gen(Statement.Initialize(name, int))
      name

    private def createName(operand: Operand): Name = operand match
      case Operand.OnInt(int) => createInt(int)
      case Operand.OnName(name) => name

/** Represents a series of statements, composing our IR
 *
 *  This is a linear IR, roughly corresponding to a Three-Address-Code
 */
case class IR(statements: Vector[IR.Statement]):
  def pprint: String =
    val buf = StringJoiner("\n")
    statements.map(_.pprint).foreach(buf.add(_))
    buf.toString
