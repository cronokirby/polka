package polka

import AST._
import Identifiers._

import org.junit.Test
import org.junit.Assert._

class ASTTest
  def ast(program: String): Either[Parser.Error, IntMain] =
    Lexer.lex(program) match
      case Left(errors) => throw AssertionError(s"Lexing Errors: $errors")
      case Right(tokens) => Parser.parse(tokens).map(AST.fromSyntax(_))

  @Test
  def `AST handles basic programs`(): Unit =
    val program = "int main() { return 100; }"
    val expected = IntMain(Vector(Statement.Return(Expr.Litteral(100))))
    assertEquals(Right(expected), ast(program))

  @Test
  def `AST handles unary operators`(): Unit =
    val program = "int main() { return !~-100; }"
    val expr = Expr.Unary(
        UnaryOp.Not,
        Expr.Unary(
          UnaryOp.BitNot,
          Expr.Unary(
            UnaryOp.Negate,
            Expr.Litteral(100))))
    val expected = IntMain(Vector(
      Statement.Return(expr)
    ))
    assertEquals(Right(expected), ast(program))

  @Test
  def `AST handles arithmetic expressions`(): Unit =
    val program = "int main() { return 1 + -1 + 3 * (4 + 2); }"
    val expected = IntMain(Vector(
      Statement.Return(Expr.Binary(
        BinOp.Add,
        Expr.Binary(
          BinOp.Add,
          Expr.Litteral(1),
          Expr.Unary(UnaryOp.Negate, Expr.Litteral(1))
        ),
        Expr.Binary(
          BinOp.Times,
          Expr.Litteral(3),
          Expr.Binary(
            BinOp.Add,
            Expr.Litteral(4),
            Expr.Litteral(2)
          )
        )
      ))
    ))
    assertEquals(Right(expected), ast(program))

  @Test
  def `AST handles single declarations`(): Unit =
    val program = "int main() { int x; int y = 2; }"
    val expected = IntMain(Vector(
      Statement.Declaration(Identifier("x"), None),
      Statement.Declaration(Identifier("y"), Some(Expr.Litteral(2)))
    ))
    assertEquals(Right(expected), ast(program))

  @Test
  def `AST handles multiple declarations`(): Unit =
    val program = "int main() { int x, y = 2; }"
     val expected = IntMain(Vector(
      Statement.Declaration(Identifier("x"), None),
      Statement.Declaration(Identifier("y"), Some(Expr.Litteral(2)))
    ))
    assertEquals(Right(expected), ast(program))

  @Test
  def `AST handles expressions with variables`(): Unit =
    val program = "int main() { return x; }"
    val expected = IntMain(Vector(
      Statement.Return(Expr.Ident(Identifier("x")))
    ))
    assertEquals(Right(expected), ast(program))

  @Test
  def `subtractions parse`: Unit =
    val program = "int main() { return 1 + 2 - 3; }"
    val expected = IntMain(Vector(
      Statement.Return(
        Expr.Binary(
          BinOp.Sub,
          Expr.Binary(
            BinOp.Add,
            Expr.Litteral(1),
            Expr.Litteral(2)
          ),
          Expr.Litteral(3)
        )
      )
    ))
    assertEquals(Right(expected), ast(program))

  @Test
  def `divisions and modulus parse`: Unit =
    val program = "int main() { return 1 / 1 % 1; }"
    val expected = IntMain(Vector(
      Statement.Return(Expr.Binary(
        BinOp.Modulo,
        Expr.Binary(
          BinOp.Divide,
          Expr.Litteral(1),
          Expr.Litteral(1)
        ),
        Expr.Litteral(1)
      ))
    ))
    assertEquals(Right(expected), ast(program))