package polka

import Syntax._

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

  enum Operand:
    case OnInt(value: Int)
    case OnName(name: Name)

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
      val name = add(program.expr) match
        case Operand.OnInt(int) => createInt(int)
        case Operand.OnName(name) => name
      gen(Statement.Return(name))
      IR(buf.toVector)

    private def add(expr: Add): Operand =
      val operands = expr.exprs.map(multiply(_))
      reduceOp(operands, BinOp.Add)

    private def multiply(expr: Multiply): Operand =
      val operands = expr.exprs.map(primExpr(_))
      reduceOp(operands, BinOp.Times)

    private def reduceOp(operands: Seq[Operand], op: BinOp): Operand =
      if operands.length == 1 then
        operands.head
      else
        val left = operands.head
        val right = operands.tail.head
        val rest = operands.tail.tail
        var name = nextName()
        gen(Statement.ApplyBin(name, op, left, right))
        for operand <- rest do
          val oldName = Operand.OnName(name)
          name = nextName()
          gen(Statement.ApplyBin(name, op, oldName, operand))
        Operand.OnName(name)

    private def primExpr(expr: PrimaryExpr): Operand =
      def go(expr: PrimaryExpr, ops: Vector[UnaryOp]): (Either[Add, Int], Vector[UnaryOp]) = expr match
        case PrimaryExpr.Litteral(int) => (Right(int), ops)
        case PrimaryExpr.Not(expr) => go(expr, ops :+ UnaryOp.Not)
        case PrimaryExpr.BitNot(expr) => go(expr, ops :+ UnaryOp.BitNot)
        case PrimaryExpr.Negate(expr) => go(expr, ops :+ UnaryOp.Negate)
        case PrimaryExpr.Parens(expr) => (Left(expr), ops)

      val (root, ops) = go(expr, Vector.empty)
      val rootOperand: Operand = root match
        case Right(int) => Operand.OnInt(int)
        case Left(expr) => add(expr)
      ops.foldLeft(rootOperand):
        (operand, op) =>
          val newName = nextName()
          val right = operand match
            case Operand.OnInt(int) => createInt(int)
            case Operand.OnName(name) => name
          gen(Statement.ApplyUnary(newName, op, right))
          Operand.OnName(newName)

    private def createInt(int: Int): Name =
      val name = nextName()
      gen(Statement.Initialize(name, int))
      name

/** Represents a series of statements, composing our IR
 *
 *  This is a linear IR, roughly corresponding to a Three-Address-Code
 */
case class IR(statements: Vector[IR.Statement]):
  def pprint: String =
    val buf = StringJoiner("\n")
    statements.map(_.pprint).foreach(buf.add(_))
    buf.toString
