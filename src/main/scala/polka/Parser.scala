package polka

import Lexer.Token
import polka.util.{Cursor, Parser => P}
import Syntax._

import scala.annotation.tailrec

object Parser
  case class Error(message: String)

  /** Given a sequence of tokens, try and parse that program into an AST
   *
   * @param tokens the series of tokens we've lexed.
   * @return either an error in parsing, or the AST
   */
  def parse(tokens: IndexedSeq[Token]): Either[Error, IntMainReturn] =
    val cursor = Cursor(tokens)
    intMainReturn.run(cursor).asEither.left.map(Error(_))

  private def intMainReturn: P[Token, IntMainReturn] =
    for
      _ <- P.litt(Token.IntType)
      _ <- P.litt(Token.Main)
      _ <- P.litt(Token.OpenParens)
      _ <- P.litt(Token.CloseParens)
      _ <- P.litt(Token.OpenBrace)
      _ <- P.litt(Token.Return)
      a <- add1
      _ <- P.litt(Token.SemiColon)
      _ <- P.litt(Token.CloseBrace)
    yield
      IntMainReturn(a)

  private def add1: P[Token, Add] = multiply1.sepBy1(Token.Plus).map(Add(_))

  private def multiply1: P[Token, Multiply] = primaryExpr1.sepBy1(Token.Times).map(Multiply(_))

  private def primaryExpr1: P[Token, PrimaryExpr] =
    import PrimaryExpr._
    def higherExpr = P.litt(Token.OpenParens) ~> add1.map(Parens(_)) <~ P.litt(Token.CloseParens)
    def not = P.litt(Token.Exclamation) ~> primaryExpr1.map(Not(_))
    def bitNot = P.litt(Token.Tilde) ~> primaryExpr1.map(BitNot(_))
    def negate = P.litt(Token.Minus) ~> primaryExpr1.map(Negate(_))
    def litteral = P.partial[Token, PrimaryExpr]:
      case Token.IntLitteral(i) => Litteral(i)
    higherExpr | not | bitNot | negate | litteral
