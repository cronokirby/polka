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

  object BinOp
    def fromSyntax(op: Syntax.BinaryOp): BinOp = op match
      case Syntax.BinaryOp.Add => Add
      case Syntax.BinaryOp.Times => Times

  /** Represents a type of unary operation on a term */
  enum UnaryOp
    /** Represents the bitwise not operator `~` */
    case BitNot
    /** Represents the arithmetic negation operator `-` */
    case Negate
    /** Represents the logical negation operator `!` */
    case Not

  object UnaryOp
      def fromSyntax(op: Syntax.UnaryOp): UnaryOp = op match
        case Syntax.UnaryOp.Not => Not
        case Syntax.UnaryOp.BitNot => BitNot
        case Syntax.UnaryOp.Negate => Negate

  /** Represents an expression evaluating to a value */
  enum Expr
    /** A binary operator between terms, e.g. `_ + _ + _` */
    case Binary(op: BinOp, left: Expr, right: Expr)
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
    decls.flatMap:
      case Syntax.InitDeclarator.Uninitialized(decl) => Vector(Statement.Declaration(decl.name, None))
      case Syntax.InitDeclarator.Initialized(decl, init) =>
        val (prelude, top) = fromPrimExpr(init)
        prelude :+ Statement.Declaration(decl.name, Some(top))

  private def fromTopExpr(expr: Syntax.TopExpr): (Vector[Statement], Expr) =
    val assigned = expr.exprs.map(fromPrimExpr(_))
    val tailPrelude = assigned.tail.flatMap((prelude, e) => prelude :+ Statement.ExprS(e))
    val (headPrelude, headExpr) = assigned.head
    (tailPrelude ++ headPrelude, headExpr)

  private def fromPrimExpr(prim: Syntax.PrimaryExpr): (Vector[Statement], Expr) =
    import Syntax.PrimaryExpr
    prim match
    case PrimaryExpr.Litteral(int) => (Vector(), Expr.Litteral(int))
    case PrimaryExpr.Ident(name) => (Vector(), Expr.Ident(name))
    case PrimaryExpr.Unary(op, term) =>
      val (prelude, term2) = fromPrimExpr(term)
      (prelude, Expr.Unary(UnaryOp.fromSyntax(op), term2))
    case PrimaryExpr.Binary(op, left, right) =>
      val (preludeL, termL) = fromPrimExpr(left)
      val (preludeR, termR) = fromPrimExpr(right)
      val expr = Expr.Binary(BinOp.fromSyntax(op), termL, termR)
      (preludeL ++ preludeR, expr)
    case PrimaryExpr.Assignment(name, expr) =>
      val (prelude, expr2) = fromPrimExpr(expr)
      (prelude, Expr.Assignment(name, expr2))
    case PrimaryExpr.Parens(expr) => fromTopExpr(expr)
