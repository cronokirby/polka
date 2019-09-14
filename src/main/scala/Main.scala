import polka.{Assembler, Lexer, Parser}
import scala.io.Source
import scala.util.Using

object Main:

  def main(args: Array[String]): Unit =
    val filename = args(0)
    val source = Source.fromFile(filename)
    val program = source.mkString
    source.close()
    Lexer(program).run() match
      case Left(err) => println(err)
      case Right(tokens) => Parser(tokens).run() match
        case Left(err) => println(err)
        case Right(program) =>
          Assembler(System.out).output(program)
