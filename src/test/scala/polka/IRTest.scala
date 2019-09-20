package polka

import IR._

import org.junit.Test
import org.junit.Assert._

class IRTest:
  def toIR(program: String): Either[Parser.Error, IR] =
    Lexer.lex(program) match
      case Left(errors) => throw AssertionError(s"Lexing Errors: $errors")
      case Right(tokens) => Parser.parse(tokens).map(IR.from(_))

  @Test
  def `ir works for constant return`(): Unit =
    val program = "int main() { return 42; }"
    val ir = toIR(program)
    val expected = IR(Vector(
      Statement.Initialize(Name(0), 42),
      Statement.Return(Name(0))
    ))
    assertEquals(Right(expected), ir)
