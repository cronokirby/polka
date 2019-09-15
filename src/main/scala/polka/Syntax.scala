package polka

/** This object holds classes related to our representation of C */
object Syntax:
  /** Represents an expression in C. */
  enum Expr:
    case Litteral(value: Int)
    /** Represents the use of logical negation `!` */
    case Not(value: Expr)
    /** Represents the use of bitwise not `~` */
    case BitNot(value: Expr)
    /** Represents the use of arithmetic negation `-` */
    case Negate(value: Expr)

  /** Represents a hardcoded `int main()` function. 
   *
   *  As our representation of the language gets richer, we'll be able
   *  to avoid this hardcoded main function construct.
   */
  case class IntMainReturn(expr: Expr)
