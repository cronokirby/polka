object Lexer {

  /** A token produced when lexing the C language. */
  enum Token {
    /** A hardcoded `int` type */
    case IntType
    /** The hardcoded `main` function name */
    case Main
    /** The `return` keyword */
    case Return
    /** The punctuation `(` */
    case OpenParens
    /** The punctuation `)` */
    case CloseParens
    /** The punctuation `{` */
    case OpenBrace
    /** The punctuation `}` */
    case CloseBrace
    /** The punctuation `;` */
    case SemiColon
    /** An integer litteral */
    case IntLitteral(value: Int)
  }
}