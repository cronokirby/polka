package polka.util

object Parser
  /** Represents the result of parsing */
  enum Reply[+T, +A]
    /** Represents a successful parser, with the remaining input */
    case Ok(value: A, rest: Cursor[T])
    /** Represents a parse failure with a given error mesage */
    case Error(msg: String)

    /** Map a function over the possible value contained in this object
     *
     *  @param fun the function to map
     *  @return a new reply with the successful value transformed
     */
    def map[B](fun: A => B): Reply[T, B] = this match
      case Error(msg) => Error(msg)
      case Ok(value, rest) => Ok(fun(value), rest)

    /** Represent this object as an Either
     *
     *  @return Right if this represents success, otherwise Left with an error
     */
    def asEither: Either[String, A] = this match
      case Ok(value, _) => Right(value)
      case Error(msg) => Left(msg)

  /** This wraps a `Reply` in order to give us information about consumption
   *
   *  Because we want to implement LL(1) parsing by default, we want to have
   *  different behavior with alternatives depending on whether or not they
   *  consumed input. For example, if an alternative has failed after already
   *  consuming input, we don't do any backtracking. We also use the consumption
   *  information with successful parsers, in order to select the longest match.
   */
  enum Result[+T, +A]
    /** We've consumed input to produce this reply */
    case Consumed(reply: Reply[T, A])
    /** We haven't consumed any input to produce this reply */
    case Empty(reply: Reply[T, A])

    /** Map a function over the reply contained inside of this object.
     *
     *  @param fun the function to map
     *  @return the same consumption information, but with the reply changed
     */
    def map[B](fun: A => B): Result[T, B] = this match
      case Consumed(r) => Consumed(r.map(fun))
      case Empty(r) => Empty(r.map(fun))

    /** Convert this function into an `Either`
     *
     *  This is useful when consuming the result of a parser, allowing us
     *  to change this module's error types into our own.
     *  @return a `Left` with an error if this result failed, otherwise `Right`
     */
    def asEither: Either[String, A] = this match
      case Consumed(r) => r.asEither
      case Empty(r) => r.asEither

  /** Create a parser that always returns a value with consuming any input.
   *
   *  @param value the value to be returned
   *  @return a new parser that always returns `value`
   */
  def returning[T, A](value: A): Parser[T, A] =
    Parser(cursor => Result.Empty(Reply.Ok(value, cursor)))

  /** Create a parser that attempts to match input against a predicate.
   *
   *  @param predicate the predicate to match
   *  @return a parser that will succeed if `predicate(token)` is true
   */
  def satisfy[T](predicate: T => Boolean): Parser[T, T] =
    val fun = (input: Cursor[T]) => input match
    case Cursor.Cons(c, rest) if predicate(c) => Result.Consumed(Reply.Ok(c, rest))
    case Cursor.Cons(c, _) => Result.Empty(Reply.Error(s"Token $c failed test"))
    case _ => Result.Empty(Reply.Error("Empty input"))
    Parser(fun)

  /** Create a parser to match a litteral token
   *
   *  This is equivalent to `satisfy(_ == token)`
   *  @param token the token to match
   *  @return a parser that succeeds when it sees exactly a token
   */
  def litt[T](token: T): Parser[T, T] = satisfy(_ == token)

  /** Create a parser that succeeds when its partial function is defined
   *
   *  @param f the function to use for matching
   *  @return a parser that succeeds when `f` is defined
   */
  def partial[T, A](f: PartialFunction[T, A]): Parser[T, A] =
    satisfy((t: T) => f.isDefinedAt(t)).map(f)

class Parser[T, A](val run: Cursor[T] => Parser.Result[T, A])
  import Parser._

  /** Map a function over the result of a parser
   *
   *  @param f the function to map
   *  @return a new parser whose result will be modified by `f`
   */
  def map[B](f: A => B): Parser[T, B] =
    Parser(input => run(input).map(f))

  /** Chain another parser using the result of this one.
   *
   *  @param f the chaining function to use
   *  @return a new parser sequencing this parser and the following
   */
  def flatMap[B](f: A => Parser[T, B]): Parser[T, B] =
    val fun = (input: Cursor[T]) => run(input) match
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

  /** Run another parser after this one, using the latter's result
   *
   *  This is lazy, to allow definitions chaining into themselves.
   *  @param that the parser to run after this
   *  @return a parser running this, and then that, returning what that returns
   */
  def ~>[B](that: => Parser[T, B]): Parser[T, B] = flatMap(_ => that)

  /** Create a new parser that runs this, and then that, and returns the first result.
   *
   *  This is like the reverse of `<~`
   *  @param that the parser to run after this
   *  @return a parser that returns our result after running that parser
   */
  def <~[B](that: Parser[T, B]): Parser[T, A] =
    for
      a <- this
      _ <- that
    yield a

  /** Create a parser that tries this, before trying that.
   *
   *  This combinator is what allows us to choose between different alternatives
   *  It's important to note that we'll only try another alternative if the first
   *  didn't consume any input. If we want to do backtracking, we need to use the `tried` method.
   *  @param that the parser to try as an alternative to this
   *  @return a parser that matches this or that
   */
  def |(that: Parser[T, A]): Parser[T, A] =
    val fun = (input: Cursor[T])=> run(input) match
      case Result.Empty(Reply.Error(msg)) => that.run(input)
      case Result.Empty(ok) => that.run(input) match
        case Result.Empty(_) => Result.Empty(ok)
        case consumed => consumed
      case consumed => consumed
    Parser(fun)

  /** Create a parser to allow backtracking
   *
   *  This works by creating a parser that calims to have consumed no input, no
   *  matter what. By doing this, we can have unlimited backtracking for a specific option.
   *  This method can be combined with `|` in order to provide alternatives that look at more
   *  than one token in advance.
   */
  def tried(): Parser[T, A] =
    val fun = (input: Cursor[T]) => run(input) match
      case Result.Consumed(Reply.Error(msg)) => Result.Empty(Reply.Error(msg))
      case other => other
    Parser(fun)

  /** Create a parser that matches this zero or many times */
  def many(): Parser[T, Vector[A]] =
    def continued = for x <- this; xs <- many() yield x +: xs
    continued | returning(Vector())

  /** Create a parser that matches this at least once */
  def many1(): Parser[T, Vector[A]] =
    for
      x <- this
      xs <- many1() | returning(Vector())
    yield x +: xs

  /** Create a parser that matches this until a special token is seen */
  def manyTill(token: T): Parser[T, Vector[A]] =
    def go: Parser[T, Vector[A]] =
      val tryEnd = litt(token).map(_ => Vector[A]())
      val tryNext = for x <- this; xs <- go yield x +: xs
      tryEnd | tryNext
    go

  /** Create a parser that matches this many times, separated by a given token*/
  def sepBy1(sep: T): Parser[T, Vector[A]] =
    for
      x <- this
      xs <- (litt(sep) ~> this).many()
    yield x +: xs
