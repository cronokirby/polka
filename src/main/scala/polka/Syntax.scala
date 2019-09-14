package polka

/** This object holds classes related to our representation of C */
object Syntax:

  /** Represents an expression in C. */
  enum Expr:
    /** Represents an litteral integer, e.g. `320` */
    case IntLitteral(value: Int)

  /** Represents a hardcoded `int main()` function. 
   *
   *  As our representation of the language gets richer, we'll be able
   *  to avoid this hardcoded main function construct.
   */
  case class IntMainReturn(expr: Expr)
