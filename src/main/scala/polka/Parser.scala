package polka

import Lexer.Token
import Syntax._

import scala.annotation.tailrec

object Parser:
  case class Error(message: String)

  /** Given a sequence of tokens, try and parse that program into an AST
   *
   * @param tokens the series of tokens we've lexed.
   * @return either an error in parsing, or the AST
   */
  def parse(tokens: Iterable[Token]): Either[Error, IntMainReturn] = Parser(tokens).run()

private class Parser(tokens: Iterable[Token]):
  import Parser._

  val iter = tokens.iterator.buffered

  def run(): Either[Error, IntMainReturn] =
    for
      _ <- eat(Token.IntType)
      _ <- eat(Token.Main)
      _ <- eat(Token.OpenParens)
      _ <- eat(Token.CloseParens)
      _ <- eat(Token.OpenBrace)
      _ <- eat(Token.Return)
      a <- add()
      _ <- eat(Token.SemiColon)
      _ <- eat(Token.CloseBrace)
    yield
      IntMainReturn(a)

  private def eat(token: Lexer.Token): Either[Error, Unit] = iter.nextOption match
    case Some(t) if t == token => Right(())
    case Some(t) => Left(Error(s"Expected $token but got $t"))
    case None => Left(Error("Unexpected end of input"))

  private def add(): Either[Error, Add] =
    def extra(buf: Vector[Multiply]): Either[Error, Vector[Multiply]] = iter.headOption match
      case Some(Token.Plus) =>
        iter.next()
        multiply().flatMap(m => extra(buf :+ m))
      case _ => Right(buf)
    for
      m <- multiply()
      ms <- extra(Vector())
    yield
      Add(m +: ms)

  private def multiply(): Either[Error, Multiply] =
    def extra(buf: Vector[PrimaryExpr]): Either[Error, Vector[PrimaryExpr]] = iter.headOption match
      case Some(Token.Times) =>
        iter.next()
        primaryExpr().flatMap(m => extra(buf :+ m))
      case _ => Right(buf)
    for
      e <- primaryExpr()
      es <- extra(Vector())
    yield
      Multiply(e +: es)

  private def primaryExpr(): Either[Error, PrimaryExpr] = iter.nextOption match
    case Some(Token.IntLitteral(i)) => Right(PrimaryExpr.Litteral(i))
    case Some(Token.Exclamation) => primaryExpr().map(PrimaryExpr.Not(_))
    case Some(Token.Tilde) => primaryExpr().map(PrimaryExpr.BitNot(_))
    case Some(Token.Minus) => primaryExpr().map(PrimaryExpr.Negate(_))
    case Some(Token.OpenParens) =>
      for
        a <- add()
        _ <- eat(Token.CloseParens)
      yield
        PrimaryExpr.Parens(a)
    case Some(t) => Left(Error(s"Expected expression, found $t"))
    case None => Left(Error("Unexpected end of input"))
