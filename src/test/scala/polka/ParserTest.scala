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
  def basicProgramParses(): Unit =
    val program = "int main() { return 100; }"
    val expected = IntMain(Vector(
      Statement.Return(Add(Vector(Multiply(Vector(PrimaryExpr.Litteral(100))))))
    ))
    assertEquals(Right(expected), parse(program))

  @Test
  def unaryOperatorsParse(): Unit =
    val program = "int main() { return !~-100; }"
    val expected = IntMain(Vector(Statement.Return(Add(Vector(
      Multiply(Vector(
        PrimaryExpr.Not(PrimaryExpr.BitNot(PrimaryExpr.Negate(PrimaryExpr.Litteral(100)))))
      ))
    ))))
    assertEquals(Right(expected), parse(program))

  @Test
  def arithmeticExpressionsParse(): Unit =
    val program = "int main() { return 1 + -1 + 3 * (4 + 2); }"
    val expected = IntMain(Vector(Statement.Return(Add(Vector(
      Multiply(Vector(PrimaryExpr.Litteral(1))),
      Multiply(Vector(PrimaryExpr.Negate(PrimaryExpr.Litteral(1)))),
      Multiply(Vector(
        PrimaryExpr.Litteral(3),
        PrimaryExpr.Parens(Add(Vector(
          Multiply(Vector(PrimaryExpr.Litteral(4))),
          Multiply(Vector(PrimaryExpr.Litteral(2)))
        )))
      ))
    )))))
    assertEquals(Right(expected), parse(program))

  @Test
  def `single declarations parse`(): Unit =
    val program = "int main() { int x; }"
    val expected = IntMain(Vector(
      Statement.Declaration(Vector(
        InitDeclarator.Uninitialized(Declarator(Identifier("x")))
      ))
    ))
    assertEquals(Right(expected), parse(program))

  @Test
  def `single declarations with initialization parse`(): Unit =
    val program = "int main() { int x = 2; }"
    val expr = Add(Vector(Multiply(Vector(PrimaryExpr.Litteral(2)))))
    val expected = IntMain(Vector(
      Statement.Declaration(Vector(
        InitDeclarator.Initialized(Declarator(Identifier("x")), expr)
      ))
    ))
    assertEquals(Right(expected), parse(program))

  @Test
  def `multiple empty declarations parse`(): Unit =
    val program = "int main() { int x, y; }"
    val expected = IntMain(Vector(
      Statement.Declaration(Vector(
        InitDeclarator.Uninitialized(Declarator(Identifier("x"))),
        InitDeclarator.Uninitialized(Declarator(Identifier("y")))
      ))
    ))
    assertEquals(Right(expected), parse(program))

  @Test
  def `multiple mixed declarations parse`(): Unit =
    val program = "int main() { int x, y, z = 2; }"
    val expr = Add(Vector(Multiply(Vector(PrimaryExpr.Litteral(2)))))
    val expected = IntMain(Vector(
      Statement.Declaration(Vector(
        InitDeclarator.Uninitialized(Declarator(Identifier("x"))),
        InitDeclarator.Uninitialized(Declarator(Identifier("y"))),
        InitDeclarator.Initialized(Declarator(Identifier("z")), expr)
      ))
    ))
    assertEquals(Right(expected), parse(program))

  @Test
  def `multiple declarations with parens work`(): Unit =
    val program = "int main() { int (x), (y), (z) = 2; }"
    val expr = Add(Vector(Multiply(Vector(PrimaryExpr.Litteral(2)))))
    val expected = IntMain(Vector(
      Statement.Declaration(Vector(
        InitDeclarator.Uninitialized(Declarator(Identifier("x"))),
        InitDeclarator.Uninitialized(Declarator(Identifier("y"))),
        InitDeclarator.Initialized(Declarator(Identifier("z")), expr)
      ))
    ))
    assertEquals(Right(expected), parse(program))

  @Test
  def `multiple statements can be parsed`(): Unit =
    val program = "int main() { return 0; return 0; }"
    val expr = Add(Vector(Multiply(Vector(PrimaryExpr.Litteral(0)))))
    val expected = IntMain(Vector(
      Statement.Return(expr),
      Statement.Return(expr)
    ))
    assertEquals(Right(expected), parse(program))

  @Test
  def `expressions containing variables can be parsed`(): Unit =
    val program = "int main() { return x; }"
    val expected = IntMain(Vector(
      Statement.Return(Add(Vector(Multiply(Vector(PrimaryExpr.Ident(Identifier("x")))))))
    ))
