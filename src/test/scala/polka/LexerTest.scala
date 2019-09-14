package polka

import Lexer.Token

import org.junit.Test
import org.junit.Assert._

class LexerTest:
  @Test
  def basicProgramLexes(): Unit =
    val program = "int main() { return 2; }"
    val items = Lexer.lex(program)
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
    println(items)
    assertEquals(Right(expected), items)