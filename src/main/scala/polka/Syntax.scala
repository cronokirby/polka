package polka

import Identifiers._

/** This object holds classes related to the parser's representation of C
 *
 *  This is a direct representation of the syntax, and not necessarily
 *  a convenient representation to have. We usually have a desugaring
 *  step after this.
 */
object Syntax
  enum PrimaryExpr
    case Litteral(value: Int)
    /** Represents the use of logical negation `!` */
    case Not(value: PrimaryExpr)
    /** Represents the use of bitwise not `~` */
    case BitNot(value: PrimaryExpr)
    /** Represents the use of arithmetic negation `-` */
    case Negate(value: PrimaryExpr)
    /** Represents the use of `()` to wrap an expression */
    case Parens(value: Add)

  /** Contains a non empty sequence of things multiplied together */
  case class Multiply(exprs: Vector[PrimaryExpr])

  /** Contains a non empty sequence of things added together */
  case class Add(exprs: Vector[Multiply])

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
    case Initialized(decl: Declarator, init: Add)

  /** Represents a statement in a block */
  enum Statement
    /** An expression statement, e.g. `2 + 2;` */
    case Expr(expr: Add)
    /** Represents a declaration, e.g. `int x = 2, y;` */
    case Declaration(declarators: Vector[InitDeclarator])
    /** Represents a return statement, e.g. `return 2 +2;` */
    case Return(expr: Add)

  /** Represents a hardcoded `int main()` function.
   *
   *  As our representation of the language gets richer, we'll be able
   *  to avoid this hardcoded main function construct.
   */
  case class IntMain(statements: Vector[Statement])
