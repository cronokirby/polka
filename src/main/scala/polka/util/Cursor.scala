package polka.util

object Cursor:
  /** Destructures an empty cursor, containing no more characters */
  object Empty:
    def unapply(cursor: Cursor): Boolean = cursor.isDone

  /** Destructures the first character, and the rest of the Cursor */
  object Cons:
    def unapply(cursor: Cursor): Option[(Char, Cursor)] =
      cursor.head.map(c => (c, cursor.advanced))

/** Represents an advanceable cursor over a String.
 *
 *  This acts like a kind of iterator over a String. This class is useful when
 *  working with parser combinators, as we want to advance over a string, while
 *  sharing this string between different results.
 *
 *  @param source the source string this cursor refers to
 *  @param pos the initial position of this cursor in that source
 */
class Cursor(private val source: String, private val pos: Int):
  /** Construct a new Cursor at the start of a source
   *
   *  @param source the source this cursor refers to
   */
  def this(source: String) = this(source, 0)

  /** Check whether or not this Cursor still has available characters */
  def isDone: Boolean = pos < 0 || pos > source.length

  /** Return a new Cursor advanced by a single character
   *
   *  For finished cursors, this does nothing.
   *
   *  @return a cursor with one less character
   */
  def advanced: Cursor = if isDone then this else Cursor(source, pos + 1)

  /** Attempt to get the first character of this Cursor
   *
   *  @return `None` for finished Cursors, otherwise a character
   */
  def head: Option[Char] = if isDone then None else Some(source.charAt(pos))

  private def slice = source.slice(pos, source.length)

  override def toString: String = s"Cursor($slice)"

  override def equals(that: Any) = that match
    case that: Cursor => slice == that.slice
    case _ => false
