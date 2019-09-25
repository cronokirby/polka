package polka

import Syntax._

import org.junit.Test
import org.junit.Assert._

class ParserTest
  def parse(program: String): Either[Parser.Error, IntMainReturn] =
    Lexer.lex(program) match
      case Left(errors) => throw AssertionError(s"Lexing Errors: $errors")
      case Right(tokens) => Parser.parse(tokens)

  @Test
  def basicProgramParses(): Unit =
    val program = "int main() { return 100; }"
    val expected: IntMainReturn = IntMainReturn(Add(Vector(Multiply(Vector(PrimaryExpr.Litteral(100))))))
    assertEquals(Right(expected), parse(program))

  @Test
  def unaryOperatorsParse(): Unit =
    val program = "int main() { return !~-100; }"
    val expected = IntMainReturn(Add(Vector(
      Multiply(Vector(
        PrimaryExpr.Not(PrimaryExpr.BitNot(PrimaryExpr.Negate(PrimaryExpr.Litteral(100)))))
      ))
    ))
    assertEquals(Right(expected), parse(program))

  @Test
  def arithmeticExpressionsParse(): Unit =
    val program = "int main() { return 1 + -1 + 3 * (4 + 2); }"
    val expected: IntMainReturn = IntMainReturn(Add(Vector(
      Multiply(Vector(PrimaryExpr.Litteral(1))),
      Multiply(Vector(PrimaryExpr.Negate(PrimaryExpr.Litteral(1)))),
      Multiply(Vector(
        PrimaryExpr.Litteral(3),
        PrimaryExpr.Parens(Add(Vector(
          Multiply(Vector(PrimaryExpr.Litteral(4))),
          Multiply(Vector(PrimaryExpr.Litteral(2)))
        )))
      ))
    )))
    assertEquals(Right(expected), parse(program))
