package polka.util

import org.junit.Test
import org.junit.Assert._

class IRTest:
  @Test
  def `empty cursor is done`(): Unit =
    assertTrue(Cursor("").isDone)

  @Test
  def `empty.advanced is still done`(): Unit =
    assertTrue(Cursor("").advanced.isDone)

  @Test
  def `head is None for empty Cursors`(): Unit =
    assertEquals(None, Cursor("").head)

  @Test
  def `head returns the first character for full Cursors`(): Unit =
    assertEquals(Some('h'), Cursor("hello").head)

  @Test
  def `advanced gets us a cursor with the next character`(): Unit =
    assertEquals(Some('e'), Cursor("hello").advanced.head)

  @Test
  def `destructuring works with Cons`(): Unit =
    Cursor("a") match
    case Cursor.Empty() => ()
    case Cursor.Cons(c, cursor) =>
      assertEquals('a', c)
      assertTrue(cursor.isDone)
