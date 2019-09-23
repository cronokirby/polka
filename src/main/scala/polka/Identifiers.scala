package polka

object Identifiers:
  /** Represents some kind, of name, e.g. the `x` in `int x` */
  opaque type Identifier = String

  object Identifier:
    /** Create a new Identifier given a string holding the same content
     *
     *  @param name the string holding the name for the new Identifier
     */
    def apply(name: String): Identifier = name
