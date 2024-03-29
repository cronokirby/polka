package polka

import IR._

import org.junit.Test
import org.junit.Assert._

class IRTest
  def toIR(program: String): Either[Parser.Error, IR] =
    Lexer.lex(program) match
      case Left(errors) => throw AssertionError(s"Lexing Errors: $errors")
      case Right(tokens) => Parser.parse(tokens).map(x => IR.from(AST.fromSyntax(x)))

  @Test
  def `ir works for constant return`(): Unit =
    val program = "int main() { return 42; }"
    val ir = toIR(program)
    val expected = IR(Vector(
      Statement.Assign(Variable.Temp(0), Operand.OnInt(42)),
      Statement.Return(Variable.Temp(0))
    ))
    assertEquals(Right(expected), ir)
