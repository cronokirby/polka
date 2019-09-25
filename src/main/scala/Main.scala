import polka.{Assembler, AST, IR, Lexer, Parser}
import java.io.{File, FileOutputStream}
import scala.io.Source
import scala.util.Using

object Main

  def main(args: Array[String]): Unit =
    val filename = args(0)
    val outname = args.lift(1)
    val source = Source.fromFile(filename)
    val program = source.mkString
    source.close()
    Lexer.lex(program) match
      case Left(err) => println(s"Lexing Errors: $err")
      case Right(tokens) => Parser.parse(tokens).map(AST.oldFromSyntax(_)) match
        case Left(err) => println(s"Parsing Error: $err $tokens")
        case Right(program) =>
          val out = outname match
            case Some(file) if file.endsWith(".pir") =>
              val prog = IR.from(program).pprint
              val out = FileOutputStream(file)
              out.write(prog.getBytes("UTF8"))
              out.write('\n')
            case Some(file) => Assembler(FileOutputStream(file)).generate(IR.from(program))
            case None => Assembler(System.out).generate(IR.from(program))
