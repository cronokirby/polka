package polka

import scala.annotation.tailrec
import scala.collection.BufferedIterator
import scala.collection.mutable.StringBuilder

object Lexer:

  /** A token produced when lexing the C language. */
  enum Token:
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

  /** Represents the type of errors the lexing stage will generate */
  case class Error(message: String)

  private class LexIterator(source: BufferedIterator[Char]) extends Iterator[Either[Error, Token]]:

    def hasNext: Boolean = source.hasNext

    def next(): Either[Error, Token] = doNext()

    @tailrec
    private def doNext(): Either[Error, Token] =
      source.head match
      case '(' => 
        source.next()
        Right(Token.OpenParens)
      case ')' =>
        source.next()
        Right(Token.CloseParens)
      case '{' =>
        source.next()
        Right(Token.OpenBrace)
      case '}' =>
        source.next()
        Right(Token.CloseBrace)
      case ';' =>
        source.next()
        Right(Token.SemiColon)
      case c if c.isLetter =>
        val word = alphanumeric()
        val matched = keyword(word)
        matched.toRight(Error(s"Unkown keyword $word"))
      case i if i.isDigit =>
        val litteral = numeric()
        Right(Token.IntLitteral(litteral))
      case _ =>
        source.next()
        doNext()

    private def numeric(): Int =
      var acc = 0
      while source.head.isDigit do
        acc = 10 * acc + source.next().asDigit
      acc

    private def alphanumeric(): String =
      val acc = StringBuilder()
      // This is fine because we've entered this function on a letter
      while source.head.isLetterOrDigit do acc.append(source.next())
      acc.toString

    private def keyword(word: String): Option[Token] =
      word match
      case "int" => Some(Token.IntType)
      case "main" => Some(Token.Main)
      case "return" => Some(Token.Return)
      case _ => None

  /** Attempt to split a program into a series of tokens.
   *
   *  @param program the text composing the program's code
   *  @return a sequence of [[Lexer.Token]], or a [[Lexer.Error]]
   */
  def lex(program: String): Either[Seq[Error], Seq[Token]] =
    val source = program.iterator.buffered
    val seq = LexIterator(source).toSeq
    val (errors, tokens) = seq.partitionMap(identity)
    if !errors.isEmpty then Left(errors) else Right(tokens)
