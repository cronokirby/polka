package polka

import Identifiers._

/** This holds types related to our second representation of C.
 *
 *  This representation comes after a desugaring step, and is more convenient
 *  for typing and other types of manipulations.
 *
 *  One big difference with this representation of the syntax is the removal of
 *  parentheses. These are represented implicitly through a hierarchy of nodes.
 *  For example, the expression `3 * (1 + 1)` is represented as
 *  `Binary(Times, Vector(3, Binary(Add, Vector(1, 1))))`, which captures the
 *  order of operations correctly.
 */
object AST
  /** This represents a type of binary operation between different terms */
  enum BinOp
    /** Represents addition `+` */
    case Add
    /** Represents multiplication `*` */
    case Times

  /** Represents a type of unary operation on a term */
  enum UnaryOp
    /** Represents the bitwise not operator `~` */
    case BitNot
    /** Represents the arithmetic negation operator `-` */
    case Negate
    /** Represents the logical negation operator `!` */
    case Not

  /** Represents an expression evaluating to a value */
  enum Expr
    /** A binary operator between terms, e.g. `_ + _ + _` */
    case Binary(op: BinOp, terms: Vector[Expr])
    /** A unary operator on a given term, e.g. `!_` */
    case Unary(op: UnaryOp, term: Expr)
    /** Represents an assignment of a name to an expression */
    case Assignment(name: Identifier, term: Expr)
    /** An integer litteral, e.g. `12` */
    case Litteral(value: Int)
    /** A reference to an identifier, e.g. `x` */
    case Ident(name: Identifier)

  /** Represents a statement that might have some effect
   *
   *  The body of our main function is composed of a series of these.
   */
  enum Statement
    /** Declare a new variable (always int, for now) with an optional value */
    case Declaration(name: Identifier, init: Option[Expr])
    /** A statement that evaluates an expression */
    case ExprS(expr: Expr)
    /** A statement returning a value from the function it's contained inside of */
    case Return(expr: Expr)

  /** Represents a hardcoded `int main()` function */
  case class IntMain(statements: Vector[Statement])

  /** Convert the parser's representation of syntax to this AST */
  def fromSyntax(syntax: Syntax.IntMain): IntMain =
    IntMain(syntax.statements.flatMap(fromStatement(_)))

  private def fromStatement(statement: Syntax.Statement): Vector[Statement] =
    statement match
    case Syntax.Statement.Expr(top) =>
      val (prelude, expr) = fromTopExpr(top)
      prelude :+ Statement.ExprS(expr)
    case Syntax.Statement.Return(top) =>
      val (prelude, expr) = fromTopExpr(top)
      prelude :+ Statement.Return(expr)
    case Syntax.Statement.Declaration(decls) => fromDeclarators(decls)

  private def fromDeclarators(decls: Vector[Syntax.InitDeclarator]): Vector[Statement] =
    decls.map:
      case Syntax.InitDeclarator.Uninitialized(decl) => Statement.Declaration(decl.name, None)
      case Syntax.InitDeclarator.Initialized(decl, init) => Statement.Declaration(decl.name, Some(fromAdd(init)))

  private def fromTopExpr(expr: Syntax.TopExpr): (Vector[Statement], Expr) =
    val assigned = expr.exprs.map(fromAssignmentExpr(_))
    val tailPrelude = assigned.tail.flatMap((prelude, e) => prelude :+ Statement.ExprS(e))
    val (headPrelude, headExpr) = assigned.head
    (tailPrelude ++ headPrelude, headExpr)

  private def fromAssignmentExpr(e: Syntax.AssignmentExpr): (Vector[Statement], Expr) = e match
    case Syntax.AssignmentExpr.Pass(add) => (Vector(), fromAdd(add))
    case Syntax.AssignmentExpr.Assignment(name, top) =>
      val (prelude, expr) = fromTopExpr(top)
      (prelude, Expr.Assignment(name, expr))

  private def fromAdd(add: Syntax.Add): Expr = add.exprs match
    case Vector(one) => fromMultiply(one)
    case more => Expr.Binary(BinOp.Add, more.map(fromMultiply(_)))

  private def fromMultiply(mul: Syntax.Multiply): Expr = mul.exprs match
    case Vector(one) => fromPrimExpr(one)
    case more => Expr.Binary(BinOp.Times, more.map(fromPrimExpr(_)))

  private def fromPrimExpr(prim: Syntax.PrimaryExpr): Expr =
    import Syntax.PrimaryExpr
    prim match
    case PrimaryExpr.Litteral(int) => Expr.Litteral(int)
    case PrimaryExpr.Ident(name) => Expr.Ident(name)
    case PrimaryExpr.BitNot(prim) => Expr.Unary(UnaryOp.BitNot, fromPrimExpr(prim))
    case PrimaryExpr.Not(prim) => Expr.Unary(UnaryOp.Not, fromPrimExpr(prim))
    case PrimaryExpr.Negate(prim) => Expr.Unary(UnaryOp.Negate, fromPrimExpr(prim))
    case PrimaryExpr.Parens(expr) => fromAdd(expr)
