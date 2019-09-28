package polka

import Identifiers._
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
    def expr = topExpr.map(Statement.Expr(_)) <~ P.litt(Token.SemiColon)
    def ret = P.litt(Token.Return) ~> topExpr.map(Statement.Return(_)) <~ P.litt(Token.SemiColon)
    def declaration = P.litt(Token.IntType) ~> initDeclarations.map(Statement.Declaration(_))
    ret | declaration | expr

  private def initDeclarations: P[Token, Vector[InitDeclarator]] =
    def uninitialized = declarator.map(InitDeclarator.Uninitialized(_))
    def initialized =
      for
        d <- declarator
        _ <- P.litt(Token.Equals)
        a <- assignmentExpr
      yield
        InitDeclarator.Initialized(d, a)
    // We need lookahead on initialized, to distinguish the following:
    // `((x)) = ...` vs `((x));`
    val initDeclarator = initialized.tried() | uninitialized
    initDeclarator.sepBy1(Token.Comma) <~ P.litt(Token.SemiColon)

  private def declarator: P[Token, Declarator] =
    def raw = P.partial[Token, Declarator]:
      case Token.Ident(i) => Declarator(i)
    def wrapped = P.litt(Token.OpenParens) ~> declarator <~ P.litt(Token.CloseParens)
    wrapped | raw

  private def topExpr: P[Token, TopExpr] =
    // Yes, `2, 2` is a valid expression
    assignmentExpr.sepBy1(Token.Comma).map(TopExpr(_))

  private def assignmentExpr: P[Token, PrimaryExpr] =
    def assignment: P[Token, PrimaryExpr] =
      for
        i <- P.partial[Token, Identifier] { case Token.Ident(i) => i }
        _ <- P.litt(Token.Equals)
        a <- assignmentExpr
      yield
        PrimaryExpr.Assignment(i, a)
    def pass = add
    // We need to lookahead to distinguish `x = _` vs `x`
    assignment.tried() | pass

  private def binOp(next: P[Token, PrimaryExpr], ops: Token*)(mkOp: Token => BinaryOp): P[Token, PrimaryExpr] =
    @tailrec
    def associateLeft(root: PrimaryExpr, rest: Vector[(Token, PrimaryExpr)]): PrimaryExpr = rest match
      case Vector() => root
      case (tok, expr) +: tail => associateLeft(PrimaryExpr.Binary(mkOp(tok), root, expr), tail)
    next.sepByVarying(ops: _*).map((root, rest) => associateLeft(root, rest))

  private def add: P[Token, PrimaryExpr] =
    binOp(multiply, Token.Plus, Token.Minus):
      case Token.Plus => BinaryOp.Add
      case _ => BinaryOp.Sub

  private def multiply: P[Token, PrimaryExpr] =
    binOp(primaryExpr, Token.Times, Token.Slash, Token.Percent):
      case Token.Times => BinaryOp.Times
      case Token.Slash => BinaryOp.Divide
      case _ => BinaryOp.Modulo

  private def unaryOp(op: UnaryOp, opToken: Token): P[Token, PrimaryExpr] =
    P.litt(opToken) ~> primaryExpr.map(PrimaryExpr.Unary(op, _))

  private def primaryExpr: P[Token, PrimaryExpr] =
    import PrimaryExpr._
    def higherExpr = P.litt(Token.OpenParens) ~> topExpr.map(Parens(_)) <~ P.litt(Token.CloseParens)
    def not = unaryOp(UnaryOp.Not, Token.Exclamation)
    def bitNot = unaryOp(UnaryOp.BitNot, Token.Tilde)
    def negate = unaryOp(UnaryOp.Negate, Token.Minus)
    def litteral = P.partial[Token, PrimaryExpr]:
      case Token.IntLitteral(i) => Litteral(i)
    def ident = P.partial[Token, PrimaryExpr]:
      case Token.Ident(name) => Ident(name)
    higherExpr | not | bitNot | negate | litteral | ident
