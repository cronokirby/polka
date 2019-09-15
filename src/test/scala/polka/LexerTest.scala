package polka

import Lexer.Token

import org.junit.Test
import org.junit.Assert._

class LexerTest:
  @Test
  def basicProgramLexes(): Unit =
    val program = "int main() { return 2; }"
    val items = Lexer(program).run()
    val expected = Seq(
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
    val items = Lexer(program).run()
    val expected = Seq(Token.IntType)
    assertEquals(Right(expected), items)

  @Test
  def singleLitteralsLex(): Unit =
    val program = "34"
    val items = Lexer(program).run()
    val expected = Seq(Token.IntLitteral(34))
    assertEquals(Right(expected), items)

  @Test
  def unaryOperatorsLex(): Unit =
    val program = "!10 ~10 -10"
    val items = Lexer(program).run()
    val expected = Seq(
      Token.Exclamation,
      Token.IntLitteral(10),
      Token.Tilde,
      Token.IntLitteral(10),
      Token.Minus,
      Token.IntLitteral(10),
    )
    assertEquals(Right(expected), items)
