package polka

import Lexer.Token
import Syntax._

object Parser:
  case class Error(message: String)

class Parser(tokens: Iterable[Token]):
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
      e <- expr()
      _ <- eat(Token.SemiColon)
      _ <- eat(Token.CloseBrace)
    yield
      IntMainReturn(e)

  private def eat(token: Lexer.Token): Either[Error, Unit] = iter.nextOption match
    case Some(t) if t == token => Right(())
    case Some(t) => Left(Error(s"Expected $token but got $t"))
    case None => Left(Error("Unexpected end of input"))

  private def expr(): Either[Error, Expr] = iter.nextOption match
    case Some(Token.IntLitteral(i)) => Right(Expr.IntLitteral(i))
    case Some(t) => Left(Error(s"Expected expression, found $t"))
    case None => Left(Error("Unexpected end of input"))
