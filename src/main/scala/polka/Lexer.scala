package polka

import scala.collection.BufferedIterator
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.StringBuilder

object Lexer
  /** A token produced when lexing the C language. */
  enum Token
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
    /** The operator `!` */
    case Exclamation
    /** The operator `~` */
    case Tilde
    /** The operator `-` */
    case Minus
    /** The operator `+` */
    case Plus
    /** The operator `*` */
    case Times
    /** The operator `=` */
    case Equals
    /** An integer litteral */
    case IntLitteral(value: Int)
    /** An identifier */
    case Identifier(value: String)

  /** Represents the type of errors the lexing stage will generate */
  case class Error(message: String)

  /** The lexing step takes some text and outputs and a serieq
   *
   * @param program the text we're trying to lex
   * @return either a sequence of [[Lexer.Token]], or [[Lexer.Error]]
   */
  def lex(program: String): Either[Vector[Error], Vector[Token]] = Lexer(program).run()

private class Lexer(program: String)
  import Lexer._

  val source = program.iterator.buffered

  def run(): Either[Vector[Error], Vector[Token]] =
    val errors = Vector.newBuilder[Error]
    val tokens = Vector.newBuilder[Token]
    var done = false
    while !done do
      advance() match
      case Some(Right(token)) => tokens += token
      case Some(Left(error)) => errors += error
      case None => done = true
    val theErrors = errors.result
    if theErrors.isEmpty then Right(tokens.result) else Left(theErrors)

  private def advance(): Option[Either[Error, Token]] =
    while source.hasNext do
      source.head match
      case '(' =>
        source.next()
        return Some(Right(Token.OpenParens))
      case ')' =>
        source.next()
        return Some(Right(Token.CloseParens))
      case '{' =>
        source.next()
        return Some(Right(Token.OpenBrace))
      case '}' =>
        source.next()
        return Some(Right(Token.CloseBrace))
      case ';' =>
        source.next()
        return Some(Right(Token.SemiColon))
      case '/' =>
        source.next()
        source.headOption match
        case Some('/') => singleLineComment()
        case Some('*') => multiLineComment()
        case _ => return Some(Left(Error("Unknown token `/`")))
      case '!' =>
        source.next()
        return Some(Right(Token.Exclamation))
      case '~' =>
        source.next()
        return Some(Right(Token.Tilde))
      case '-' =>
        source.next()
        return Some(Right(Token.Minus))
      case '+' =>
        source.next()
        return Some(Right(Token.Plus))
      case '*' =>
        source.next()
        return Some(Right(Token.Times))
      case '=' =>
        source.next()
        return Some(Right(Token.Equals))
      case c if c.isLetter =>
        val word = alphanumeric()
        val matched = keyword(word).getOrElse(Token.Identifier(word))
        return Some(Right(matched))
      case i if i.isDigit =>
        val litteral = numeric()
        return Some(Right(Token.IntLitteral(litteral)))
      case _ =>
        source.next()
    None

  private def numeric(): Int =
    var acc = 0
    while source.hasNext && source.head.isDigit do
      acc = 10 * acc + source.next().asDigit
    acc

  private def alphanumeric(): String =
    val acc = StringBuilder()
    // This is fine because we've entered this function on a letter
    while source.hasNext && source.head.isLetterOrDigit do acc.append(source.next())
    acc.toString

  private def keyword(word: String): Option[Token] = word match
    case "int" => Some(Token.IntType)
    case "main" => Some(Token.Main)
    case "return" => Some(Token.Return)
    case _ => None

  private def singleLineComment(): Unit =
    while source.hasNext && source.head != '\n' do source.next()

  private def multiLineComment(): Unit =
    var stage = 0
    while source.hasNext do
      (stage, source.next()) match
      case (_, '*') => stage = 1
      case (1, '/') => return
      case _ => stage = 0
