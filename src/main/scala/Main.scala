import polka.Lexer
import scala.io.Source

object Main:

  def main(args: Array[String]): Unit =
    val filename = args(0)
    val source = Source.fromFile(filename)
    val program = source.mkString
    source.close()
    println(Lexer(program).run())
