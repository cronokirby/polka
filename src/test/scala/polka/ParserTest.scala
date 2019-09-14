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
    val program = "int main() { return bar; }"
    val expected = IntMainReturn(Expr.IntLitteral(100))
    assertEquals(Right(expected), parse(program))
