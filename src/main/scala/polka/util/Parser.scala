package polka.util

object Parser:
  enum Reply[+T, +A]:
    case Ok(value: A, rest: Cursor[T])
    case Error(msg: String)

    def map[B](fun: A => B): Reply[T, B] = this match
      case Error(msg) => Error(msg)
      case Ok(value, rest) => Ok(fun(value), rest)

  enum Result[+T, +A]:
    case Consumed(reply: Reply[T, A])
    case Empty(reply: Reply[T, A])

    def map[B](fun: A => B): Result[T, B] = this match
      case Consumed(r) => Consumed(r.map(fun))
      case Empty(r) => Empty(r.map(fun))

  def returning[A](value: A): Parser[_, A] =
    Parser(cursor => Result.Empty(Reply.Ok(value, cursor)))

  def satisfy[T](predicate: T => Boolean): Parser[T, T] =
    val fun = input: Cursor[T] => input match
    case Cursor.Empty => Result.Empty(Reply.Error("Empty input"))
    case Cursor.Cons(c, rest) if predicate(c) => Result.Consumed(Reply.Ok(c, rest))
    case Cursor.Cons(c, _) => Result.Empty(Reply.Error(s"Character $c failed test"))
    Parser(fun)

class Parser[T, A](val run: Cursor[T] => Parser.Result[T, A]):
  import Parser._

  def map[B](f: A => B): Parser[T, B] =
    Parser(input => run(input).map(f))

  def flatMap[B](f: A => Parser[T, B]): Parser[T, B] =
    val fun = input: Cursor[T] => run(input) match
    case Result.Empty(r) => r match
      case Reply.Ok(value, rest) => f(value).run(rest)
      case Reply.Error(msg) => Result.Empty(Reply.Error(msg))
    case Result.Consumed(r) =>
      lazy val reply = r match
      case Reply.Ok(value, rest) => f(value).run(rest) match
        case Result.Consumed(r) => r
        case Result.Empty(r) => r
      case Reply.Error(msg) => Reply.Error(msg)
      Result.Consumed(reply)
    Parser(fun)

  def or(that: Parser[T, A]): Parser[T, A] =
    val fun = input: Cursor[T] => run(input) match
      case Result.Empty(Reply.Error(msg)) => that.run(input)
      case Result.Empty(ok) => that.run(input) match
        case Result.Empty(_) => Result.Empty(ok)
        case consumed => consumed
      case consumed => consumed
    Parser(fun)

  def tried(): Parser[T, A] =
    val fun = input: Cursor[T] => run(input) match
      case Result.Consumed(Reply.Error(msg)) => Result.Empty(Reply.Error(msg))
      case other => other
    Parser(fun)
