package polka

import Identifiers._
import Lexer.Token

import org.junit.Test
import org.junit.Assert._

class LexerTest
  @Test
  def basicProgramLexes(): Unit =
    val program = "int main() { return 2; }"
    val items = Lexer.lex(program)
    val expected = Vector(
      Token.IntType,
      Token.Main,
      Token.OpenParens,
      Token.CloseParens,
      Token.OpenBrace,
      Token.Return,
      Token.IntLitteral(2),
      Token.SemiColon,
      Token.CloseBrace,
    )
    assertEquals(Right(expected), items)

  @Test
  def programsWithCommentsLex(): Unit =
    val program = "// stuff\n/* **stuff**/int"
    val items = Lexer.lex(program)
    val expected = Vector(Token.IntType)
    assertEquals(Right(expected), items)

  @Test
  def singleLitteralsLex(): Unit =
    val program = "34"
    val items = Lexer.lex(program)
    val expected = Vector(Token.IntLitteral(34))
    assertEquals(Right(expected), items)

  @Test
  def unaryOperatorsLex(): Unit =
    val program = "!10 ~10 -10"
    val items = Lexer.lex(program)
    val expected = Vector(
      Token.Exclamation,
      Token.IntLitteral(10),
      Token.Tilde,
      Token.IntLitteral(10),
      Token.Minus,
      Token.IntLitteral(10),
    )
    assertEquals(Right(expected), items)

  @Test
  def operatorsLex(): Unit =
    val program = "+ *"
    val items = Lexer.lex(program)
    val expected = Vector(Token.Plus, Token.Times)
    assertEquals(Right(expected), items)

  @Test
  def variablesLex(): Unit =
    val program = "int x1 = y"
    val items = Lexer.lex(program)
    val expected = Vector(
      Token.IntType,
      Token.Ident(Identifier("x1")),
      Token.Equals,
      Token.Ident(Identifier("y"))
    )
    assertEquals(Right(expected), items)

  @Test
  def `division and modules lex`: Unit =
    val program = "/ %"
    val items = Lexer.lex(program)
    val expected = Vector(
      Token.Slash,
      Token.Percent
    )
