package polka

import Syntax._

import org.junit.Test
import org.junit.Assert._

class ParserTest:
  def parse(program: String): Either[Parser.Error, IntMainReturn] =
    Lexer(program).run() match
      case Left(errors) => throw AssertionError(s"Lexing Errors: $errors")
      case Right(tokens) => Parser(tokens).run()

  @Test
  def basicProgramParses(): Unit =
    val program = "int main() { return 100; }"
    val expected = IntMainReturn(Expr.Litteral(100))
    assertEquals(Right(expected), parse(program))

  @Test
  def unaryOperatorsParse(): Unit =
    val program = "int main() { return !~-100; }"
    val expected = IntMainReturn(Expr.Not(Expr.BitNot(Expr.Negate(Expr.Litteral(100)))))
