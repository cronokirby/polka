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
  def parse(tokens: IndexedSeq[Token]): Either[Error, IntMain] =
    val cursor = Cursor(tokens)
    intMainReturn.run(cursor).asEither.left.map(Error(_))

  def oldParse(tokens: IndexedSeq[Token]): Either[Error, IntMainReturn] = Left(Error("don't use oldParse"))

  private def intMainReturn: P[Token, IntMain] =
    for
      _ <- P.litt(Token.IntType)
      _ <- P.litt(Token.Main)
      _ <- P.litt(Token.OpenParens)
      _ <- P.litt(Token.CloseParens)
      b <- block
    yield
      IntMain(b)

  private def block: P[Token, Vector[Statement]] =
    P.litt(Token.OpenBrace) ~> statement.manyTill(Token.CloseBrace)

  private def statement: P[Token, Statement] =
    def expr = add.map(Statement.Expr(_)) <~ P.litt(Token.SemiColon)
    def ret = P.litt(Token.Return) ~> add.map(Statement.Return(_)) <~ P.litt(Token.SemiColon)
    def declaration = P.litt(Token.IntType) ~> initDeclarations.map(Statement.Declaration(_))
    ret | declaration | expr

  private def initDeclarations: P[Token, Vector[InitDeclarator]] =
    def uninitialized = declarator.map(InitDeclarator.Uninitialized(_))
    def initialized =
      for
        d <- declarator
        _ <- P.litt(Token.Equals)
        a <- add
      yield
        InitDeclarator.Initialized(d, a)
    // We need lookahead on uninitialized, to distinguish the following:
    // `((x)) = ...` vs `((x));`
    val initDeclarator = uninitialized.tried() | initialized
    initDeclarator.sepBy1(Token.Comma) <~ P.litt(Token.SemiColon)

  private def declarator: P[Token, Declarator] =
    def raw = P.partial[Token, Declarator]:
      case Token.Ident(i) => Declarator.Ident(i)
    def wrapped = P.litt(Token.OpenParens) ~> declarator <~ P.litt(Token.CloseParens)
    wrapped | raw

  private def add: P[Token, Add] = multiply.sepBy1(Token.Plus).map(Add(_))

  private def multiply: P[Token, Multiply] = primaryExpr.sepBy1(Token.Times).map(Multiply(_))

  private def primaryExpr: P[Token, PrimaryExpr] =
    import PrimaryExpr._
    def higherExpr = P.litt(Token.OpenParens) ~> add.map(Parens(_)) <~ P.litt(Token.CloseParens)
    def not = P.litt(Token.Exclamation) ~> primaryExpr.map(Not(_))
    def bitNot = P.litt(Token.Tilde) ~> primaryExpr.map(BitNot(_))
    def negate = P.litt(Token.Minus) ~> primaryExpr.map(Negate(_))
    def litteral = P.partial[Token, PrimaryExpr]:
      case Token.IntLitteral(i) => Litteral(i)
    higherExpr | not | bitNot | negate | litteral
