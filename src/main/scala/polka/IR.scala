package polka

import AST._
import Identifiers._

import java.util.StringJoiner
import scala.annotation.tailrec
import scala.collection.mutable.Buffer

object IR
  /** Represents a variable in the IR */
  enum Variable
    /** Represents a temporary variable in the IR.
    *
    *  There are some rules governing when these can be used:
    *  - They can only appear on the left side of a statement once
    *  - They can only appear on the right side of a statement once
    *
    * @param index the index of this name
    */
    case Temp(index: Int)
    /** Represents a permanent variable
     *
     *  Unlike temporary variables, this need to be explicitly created, and
     *  can be used multiple times.
     */
    case Perm(ident: Identifier)

    def pprint: String = this match
      case Temp(i) => s"#$i"
      case Perm(ident) => s"$ident"

    def isTemp: Boolean = this match
      case Temp(_) => true
      case Perm(_) => false

  /** Represents a binary operation between two operands */
  enum BinOp
    /** Add the two operands using `+` */
    case Add
    /** Subtract the left operand from the right with `-` */
    case Sub
    /** Add the two operands using `*` */
    case Times
    /** Get the result of dividing two operands using `/` */
    case Divide
    /** Get the modulus of two operands using `%` */
    case Modulo

    def pprint: String = this match
      case Add => "+"
      case Sub => "-"
      case Times => "*"
      case Divide => "/"
      case Modulo => "%"

  object BinOp
    def fromAST(op: AST.BinOp): BinOp = op match
      case AST.BinOp.Add => Add
      case AST.BinOp.Sub => Sub
      case AST.BinOp.Times => Times
      case AST.BinOp.Divide => Divide
      case AST.BinOp.Modulo => Modulo

  /** Represents a unary operation against a single operand */
  enum UnaryOp
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

  object UnaryOp
    def fromAST(op: AST.UnaryOp): UnaryOp = op match
      case AST.UnaryOp.Not => UnaryOp.Not
      case AST.UnaryOp.BitNot => UnaryOp.BitNot
      case AST.UnaryOp.Negate => UnaryOp.Negate

  enum Operand
    case OnInt(value: Int)
    case OnVar(variable: Variable)

    def isTemp: Boolean = this match
      case OnVar(v) => v.isTemp
      case OnInt(_) => false

    def pprint: String = this match
      case OnInt(int) => int.toString
      case OnVar(variable) => variable.pprint

  /** Represents a TAC statement */
  enum Statement
    /** Represents a unary operation on a given variable */
    case ApplyUnary(to: Variable, op: UnaryOp, single: Variable)
    /** Represents a binary operation between two operands */
    case ApplyBin(to: Variable, op: BinOp, left: Operand, right: Operand)
    /** Initialize a given variable with a value */
    case Assign(name: Variable, as: Operand)
    /** Create a new permanent variable */
    case Create(ident: Identifier)
    /** Return the value in a variable */
    case Return(variable: Variable)

    def pprint: String = this match
      case ApplyUnary(to, op, single) => s"${to.pprint} = ${op.pprint}${single.pprint}"
      case ApplyBin(to, op, l, r) => s"${to.pprint} = ${l.pprint} ${op.pprint} ${r.pprint}"
      case Assign(name, as) => s"${name.pprint} = ${as.pprint}"
      case Create(ident) => s"create $ident"
      case Return(value: Variable) => s"ret ${value.pprint}"

  def from(program: IntMain): IR = Generator().from(program)

  class Generator
    val buf = Buffer[Statement]()
    var tempName = 0

    private def nextTemp(): Variable =
      val name = Variable.Temp(tempName)
      tempName += 1
      name

    private def gen(stmt: Statement): Unit = buf += stmt

    def from(program: IntMain): IR =
      program.statements.foreach(statement(_))
      IR(buf.toVector)

    private def statement(stmt: AST.Statement): Unit = stmt match
      case AST.Statement.Declaration(name, init) =>
        gen(Statement.Create(name))
        val variable = Variable.Perm(name)
        init.foreach(e => gen(Statement.Assign(variable, expr(e))))
      // This will generate all the parts of this expression causing side effects
      case AST.Statement.ExprS(e) => expr(e)
      case AST.Statement.Return(e) =>
        val name = expr(e) match
          case Operand.OnInt(int) => createInt(int)
          case Operand.OnVar(v) => v
        gen(Statement.Return(name))

    private def expr(e: Expr): Operand = e match
      case Expr.Litteral(int) => Operand.OnInt(int)
      case Expr.Ident(name) => Operand.OnVar(Variable.Perm(name))
      case Expr.Assignment(name, term) =>
        val operand = expr(term)
        gen(Statement.Assign(Variable.Perm(name), operand))
        Operand.OnVar(Variable.Perm(name))
      case Expr.Unary(op, term) =>
        val name = createVariable(expr(term))
        val next = nextTemp()
        gen(Statement.ApplyUnary(next, UnaryOp.fromAST(op), name))
        Operand.OnVar(next)
      case root@Expr.Binary(op, _, _) =>
        val irOp = BinOp.fromAST(op)
        @tailrec
        def gatherRight(next: Expr, acc: Vector[Expr]): Vector[Expr] = next match
          case Expr.Binary(op2, left, right) if op2 == op =>
            gatherRight(left, right +: acc)
          case something => something +: acc
        val exprs = gatherRight(root, Vector())
        exprs.tail.foldLeft(expr(exprs.head)):
          (operand, right) =>
            var tmp = nextTemp()
            gen(Statement.ApplyBin(tmp, irOp, operand, expr(right)))
            Operand.OnVar(tmp)

    private def createInt(int: Int): Variable =
      val variable = nextTemp()
      gen(Statement.Assign(variable, Operand.OnInt(int)))
      variable

    private def createVariable(operand: Operand): Variable = operand match
      case Operand.OnInt(int) => createInt(int)
      case Operand.OnVar(variable) => variable

/** Represents a series of statements, composing our IR
 *
 *  This is a linear IR, roughly corresponding to a Three-Address-Code
 */
case class IR(statements: Vector[IR.Statement])
  def pprint: String =
    val buf = StringJoiner("\n")
    statements.map(_.pprint).foreach(buf.add(_))
    buf.toString
