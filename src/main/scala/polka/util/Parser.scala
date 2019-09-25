package polka.util

object Parser:
  enum Reply[+T, +A]:
    case Ok(value: A, rest: Cursor[T])
    case Error(msg: String)

    def map[B](fun: A => B): Reply[T, B] = this match
      case Error(msg) => Error(msg)
      case Ok(value, rest) => Ok(fun(value), rest)

    def asEither: Either[String, A] = this match
      case Ok(value, _) => Right(value)
      case Error(msg) => Left(msg)

  enum Result[+T, +A]:
    case Consumed(reply: Reply[T, A])
    case Empty(reply: Reply[T, A])

    def map[B](fun: A => B): Result[T, B] = this match
      case Consumed(r) => Consumed(r.map(fun))
      case Empty(r) => Empty(r.map(fun))

    def asEither: Either[String, A] = this match
      case Consumed(r) => r.asEither
      case Empty(r) => r.asEither

  def returning[T, A](value: A): Parser[T, A] =
    Parser(cursor => Result.Empty(Reply.Ok(value, cursor)))

  def satisfy[T](predicate: T => Boolean): Parser[T, T] =
    val fun = input: Cursor[T] => input match
    case Cursor.Cons(c, rest) if predicate(c) => Result.Consumed(Reply.Ok(c, rest))
    case Cursor.Cons(c, _) => Result.Empty(Reply.Error(s"Token $c failed test"))
    case _ => Result.Empty(Reply.Error("Empty input"))
    Parser(fun)

  def litt[T](token: T): Parser[T, T] = satisfy(_ == token)

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

  def ~>[B](that: Parser[T, B]): Parser[T, B] = flatMap(_ => that)

  def <~[B](that: Parser[T, B]): Parser[T, A] =
    for
      a <- this
      _ <- that
    yield a

  def |(that: Parser[T, A]): Parser[T, A] =
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

  def many(): Parser[T, Vector[A]] =
    def continued = for x <- this; xs <- many() yield x +: xs
    continued | returning(Vector())

  def many1(): Parser[T, Vector[A]] =
    for
      x <- this
      xs <- many1() | returning(Vector())
    yield x +: xs

  def manyTill(token: T): Parser[T, Vector[A]] =
    def go: Parser[T, Vector[A]] =
      val tryEnd = litt(token).map(_ => Vector[A]())
      val tryNext = for x <- this; xs <- go yield x +: xs
      tryEnd | tryNext
    go

  def sepBy1(sep: T): Parser[T, Vector[A]] =
    for
      x <- this
      xs <- (litt(sep) ~> this).many()
    yield x +: xs
