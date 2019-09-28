package polka

import Identifiers._

/** This object holds classes related to the parser's representation of C
 *
 *  This is a direct representation of the syntax, and not necessarily
 *  a convenient representation to have. We usually have a desugaring
 *  step after this.
 */
object Syntax
  /** Represents at type of unary operation */
  enum UnaryOp
    /** Represents logical negation with `!` */
    case Not
    /** Represents bitwise negation, e.g. `~` */
    case BitNot
    /** Represents arithmetic negation, e.g. `-` */
    case Negate

  /** Represents a type of binary operation */
  enum BinaryOp
    /** The addition operator `+` */
    case Add
    /** The subtraction operator `-` */
    case Sub
    /** The multiplicatoin operator `*` */
    case Times

  enum PrimaryExpr
    /** Represents a litteral, e.g. `2` */
    case Litteral(value: Int)
    /** Represents a reference to a variable */
    case Ident(name: Identifier)
    /** Represents a unary operation */
    case Unary(op: UnaryOp, term: PrimaryExpr)
    /** Represents a binary operation */
    case Binary(op: BinaryOp, left: PrimaryExpr, right: PrimaryExpr)
    /** Represents an assignment of an identifier to some expression */
    case Assignment(name: Identifier, expr: PrimaryExpr)
    /** Represents the use of `()` to wrap an expression */
    case Parens(value: TopExpr)

  /** Represents the entry point for expressions in C */
  case class TopExpr(exprs: Vector[PrimaryExpr])

  /** Represents the name of a declaration.
   *
   *  Roughly corresponds to the `direct-declarator` rule
   */
  case class Declarator(name: Identifier)

  /** Represents a declaration after a type */
  enum InitDeclarator
    /** A declaration without initialization, e.g. `x` */
    case Uninitialized(decl: Declarator)
    /** A declaration with initialization, e.g. `x = 2` */
    case Initialized(decl: Declarator, init: PrimaryExpr)

  /** Represents a statement in a block */
  enum Statement
    /** An expression statement, e.g. `2 + 2;` */
    case Expr(expr: TopExpr)
    /** Represents a declaration, e.g. `int x = 2, y;` */
    case Declaration(declarators: Vector[InitDeclarator])
    /** Represents a return statement, e.g. `return 2 +2;` */
    case Return(expr: TopExpr)

  /** Represents a hardcoded `int main()` function.
   *
   *  As our representation of the language gets richer, we'll be able
   *  to avoid this hardcoded main function construct.
   */
  case class IntMain(statements: Vector[Statement])
