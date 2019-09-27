package polka

import Identifiers._
import Syntax._

import org.junit.Test
import org.junit.Assert._

class ParserTest
  def parse(program: String): Either[Parser.Error, IntMain] =
    Lexer.lex(program) match
      case Left(errors) => throw AssertionError(s"Lexing Errors: $errors")
      case Right(tokens) => Parser.parse(tokens)

  @Test
  def basicProgramParses: Unit =
    val program = "int main() { return 100; }"
    val expected = IntMain(Vector(
      Statement.Return(TopExpr(Vector(PrimaryExpr.Litteral(100))))
    ))
    assertEquals(Right(expected), parse(program))

  @Test
  def unaryOperatorsParse: Unit =
    val program = "int main() { return !~-100; }"
    val expr = PrimaryExpr.Unary(UnaryOp.Not,
      PrimaryExpr.Unary(UnaryOp.BitNot,
        PrimaryExpr.Unary(UnaryOp.Negate, PrimaryExpr.Litteral(100))
      )
    )
    val expected = IntMain(Vector(Statement.Return(TopExpr(Vector(expr)))))
    assertEquals(Right(expected), parse(program))

  @Test
  def arithmeticExpressionsParse: Unit =
    val program = "int main() { return 1 + -1 + 3 * (4 + 2); }"
    val expected = IntMain(Vector(Statement.Return(TopExpr(Vector(
      PrimaryExpr.Binary(
        BinaryOp.Add,
        PrimaryExpr.Litteral(1),
        PrimaryExpr.Binary(
          BinaryOp.Add,
          PrimaryExpr.Unary(UnaryOp.Negate, PrimaryExpr.Litteral(1)),
          PrimaryExpr.Binary(
            BinaryOp.Times,
            PrimaryExpr.Litteral(3),
            PrimaryExpr.Parens(TopExpr(Vector(
              PrimaryExpr.Binary(
                BinaryOp.Add,
                PrimaryExpr.Litteral(4),
                PrimaryExpr.Litteral(2)
              )
            )))
          )
        )
      )
    )))))
    assertEquals(Right(expected), parse(program))

  @Test
  def `single declarations parse`: Unit =
    val program = "int main() { int x; }"
    val expected = IntMain(Vector(
      Statement.Declaration(Vector(
        InitDeclarator.Uninitialized(Declarator(Identifier("x")))
      ))
    ))
    assertEquals(Right(expected), parse(program))

  @Test
  def `single declarations with initialization parse`: Unit =
    val program = "int main() { int x = 2; }"
    val expr = PrimaryExpr.Litteral(2)
    val expected = IntMain(Vector(
      Statement.Declaration(Vector(
        InitDeclarator.Initialized(Declarator(Identifier("x")), expr)
      ))
    ))
    assertEquals(Right(expected), parse(program))

  @Test
  def `multiple empty declarations parse`: Unit =
    val program = "int main() { int x, y; }"
    val expected = IntMain(Vector(
      Statement.Declaration(Vector(
        InitDeclarator.Uninitialized(Declarator(Identifier("x"))),
        InitDeclarator.Uninitialized(Declarator(Identifier("y")))
      ))
    ))
    assertEquals(Right(expected), parse(program))

  @Test
  def `multiple mixed declarations parse`: Unit =
    val program = "int main() { int x, y, z = 2; }"
    val expr = PrimaryExpr.Litteral(2)
    val expected = IntMain(Vector(
      Statement.Declaration(Vector(
        InitDeclarator.Uninitialized(Declarator(Identifier("x"))),
        InitDeclarator.Uninitialized(Declarator(Identifier("y"))),
        InitDeclarator.Initialized(Declarator(Identifier("z")), expr)
      ))
    ))
    assertEquals(Right(expected), parse(program))

  @Test
  def `multiple initialized declarations parse`: Unit =
    val program = "int main() { int x = 2, y = 2; }"
    val expr = PrimaryExpr.Litteral(2)
    val expected = IntMain(Vector(
      Statement.Declaration(Vector(
        InitDeclarator.Initialized(Declarator(Identifier("x")), expr),
        InitDeclarator.Initialized(Declarator(Identifier("y")), expr)
      ))
    ))
    assertEquals(Right(expected), parse(program))

  @Test
  def `multiple declarations with parens work`: Unit =
    val program = "int main() { int (x), (y), (z) = 2; }"
    val expr = PrimaryExpr.Litteral(2)
    val expected = IntMain(Vector(
      Statement.Declaration(Vector(
        InitDeclarator.Uninitialized(Declarator(Identifier("x"))),
        InitDeclarator.Uninitialized(Declarator(Identifier("y"))),
        InitDeclarator.Initialized(Declarator(Identifier("z")), expr)
      ))
    ))
    assertEquals(Right(expected), parse(program))

  @Test
  def `multiple statements can be parsed`: Unit =
    val program = "int main() { return 0; return 0; }"
    val expr = TopExpr(Vector(PrimaryExpr.Litteral(0)))
    val expected = IntMain(Vector(
      Statement.Return(expr),
      Statement.Return(expr)
    ))
    assertEquals(Right(expected), parse(program))

  @Test
  def `expressions containing variables can be parsed`: Unit =
    val program = "int main() { return x; }"
    val expr = TopExpr(Vector(PrimaryExpr.Ident(Identifier("x"))))
    val expected = IntMain(Vector(
      Statement.Return(expr)
    ))
    assertEquals(Right(expected), parse(program))

  @Test
  def `assignment expressions parse`: Unit =
    val program = "int main() { x = y = 1, z = 1; }"
    val one = PrimaryExpr.Litteral(1)
    val yEquals = PrimaryExpr.Assignment(Identifier("y"), one)
    val zEquals = PrimaryExpr.Assignment(Identifier("z"), one)
    val xEquals = PrimaryExpr.Assignment(Identifier("x"), yEquals)
    val expected = IntMain(Vector(
      Statement.Expr(TopExpr(Vector(xEquals, zEquals)))
    ))
    assertEquals(Right(expected), parse(program))

  @Test
  def `comma seperated expressions are valid`: Unit =
    val program = "int main() { 2, 2; }"
    val two = PrimaryExpr.Litteral(2)
    val expected = IntMain(Vector(
      Statement.Expr(TopExpr(Vector(two, two)))
    ))
    assertEquals(Right(expected), parse(program))
