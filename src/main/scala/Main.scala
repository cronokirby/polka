import polka.{Lexer, Parser}
import scala.io.Source

object Main:

  def main(args: Array[String]): Unit =
    val filename = args(0)
    val source = Source.fromFile(filename)
    val program = source.mkString
    source.close()
    Lexer(program).run() match
      case Left(err) => println(err)
      case Right(tokens) => println(Parser(tokens).run())
