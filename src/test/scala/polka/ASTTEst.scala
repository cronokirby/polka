package polka

import AST._

import org.junit.Test
import org.junit.Assert._

class ASTTest
  def ast(program: String): Either[Parser.Error, IntMainReturn] =
    Lexer.lex(program) match
      case Left(errors) => throw AssertionError(s"Lexing Errors: $errors")
      case Right(tokens) => Parser.oldParse(tokens).map(AST.fromSyntax(_))

  @Test
  def `AST handles basic programs`(): Unit =
    val program = "int main() { return 100; }"
    val expected = IntMainReturn(Expr.Litteral(100))
    assertEquals(Right(expected), ast(program))

  @Test
  def `AST handles unary operators`(): Unit =
    val program = "int main() { return !~-100; }"
    val expected = IntMainReturn(
      Expr.Unary(
        UnaryOp.Not,
        Expr.Unary(
          UnaryOp.BitNot,
          Expr.Unary(
            UnaryOp.Negate,
            Expr.Litteral(100))))
    )
    assertEquals(Right(expected), ast(program))

  @Test
  def `AST handles arithmetic expressions`(): Unit =
    val program = "int main() { return 1 + -1 + 3 * (4 + 2); }"
    val expected = IntMainReturn(
      Expr.Binary(
        BinOp.Add,
        Vector(
          Expr.Litteral(1),
          Expr.Unary(UnaryOp.Negate, Expr.Litteral(1)),
          Expr.Binary(
            BinOp.Times,
            Vector(
              Expr.Litteral(3),
              Expr.Binary(
                BinOp.Add,
                Vector(Expr.Litteral(4), Expr.Litteral(2))
              )
            )
          )
        )
      )
    )
    assertEquals(Right(expected), ast(program))
