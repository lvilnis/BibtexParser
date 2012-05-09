package bib

object AST {

  private[bib] sealed trait Entry
  private[bib] sealed trait Value

  final case class Document(entries: List[Entry])

  final case class StringEntry(abbrev: String, value: Value) extends Entry
  final case class PreambleEntry(content: Value) extends Entry
  final case class CommentEntry(comment: String) extends Entry
  final case class RegularEntry(
    ty: String, citationKey: String, tags: List[(String, Value)]) extends Entry

  final case class Literal(content: String) extends Value
  final case class Abbrev(name: String) extends Value
  final case class Concat(left: Value, right: Value) extends Value

}

object Parser {

  import AST._

  def parseString(input: String): Option[Document] = {
    val res = ParserImpl.parseAll(ParserImpl.bibTex, input)
    res.map(r => Some(Document(r))).getOrElse(None)
  }

  def lexNameFragments(namesString: String): List[Names.Token] =
    ParserImpl.parseAll(ParserImpl.NameLexer.nameLexer, namesString).getOrElse(Nil)

  import scala.util.parsing.combinator._

  private[bib] object ParserImpl extends RegexParsers {

    override val skipWhitespace = false

    def bibTex =
      (freeComment ~! anyEntry ~! freeComment).+ ^^
      (_ flatMap { case x ~ y ~ z => List(x, y, z): List[Entry] })

    def freeComment = "[^@]*" ^^ (CommentEntry(_))

    def anyEntry = AT ~> (commentEntry | stringEntry | preambleEntry | regularEntry)

    def commentEntry =
      COMMENT ~> WS ~> (('{' ~> "[^}]*" <~ '}') | ('(' ~> "[^\\)]*" <~ ')')) ^^
      (CommentEntry(_))

    def stringEntry = STRING ~> WS ~> entryBody { tag } ^^ (StringEntry(_, _)).tupled

    def preambleEntry = PREAMBLE ~> WS ~> entryBody { value } ^^ (PreambleEntry(_))

    def regularEntry =
      (SYMBOL <~ WS) ~ entryBody { SYMBOL ~ rep(COMMA_WS ~> tag) <~ (COMMA_WS ?) } ^^ {
        case ty ~ (key ~ tags) => RegularEntry(ty, key, tags)
      }

    def entryBody[T](parser: => Parser[T]): Parser[T] = {
      lazy val p = parser
      ("\\{\\s*" ~> p <~ "\\s*\\}") |
      ("\\(\\s*" ~> p <~ "\\s*\\)")
    }

    def tag = (SYMBOL <~ "\\s*=\\s*") ~ value ^^ {
      case sym ~ value => (sym, value)
    }

    def value: Parser[Value] = literalOrSymbol ~ ("\\s*#\\s*" ~> value).? ^^ {
      case left ~ Some(right) => Concat(left, right)
      case left ~ _ => left
    }

    def literalOrSymbol = (SYMBOL ^^ (Abbrev(_))) | literal

    def literal = (numericLiteral | braceDelimitedStringLiteral | quoteDelimitedStringLiteral)

    def numericLiteral = "\\d+(\\.\\d+)?" ^^ (Literal(_))
    def quoteDelimitedStringLiteral = '"' ~> "[^\"]*" <~ '"' ^^ (Literal(_))
    def braceDelimitedStringLiteral = braceDelimitedString ^^ (Literal(_))

    def braceDelimitedString: Parser[String] =
      '{' ~> (braceDelimitedString ^^ ("{" + _ + "}") | "[^}{]+").* <~ '}' ^^ (_.mkString)

    def AT = c('@')
    def WS = r("\\s*")
    def COMMA_WS = r("\\s*,\\s*")
    def COMMENT = r("(c|C)(o|O)(m|M)(m|M)(e|E)(n|N)(t|T)")
    def STRING = r("(s|S)(t|T)(r|R)(i|I)(n|N)(g|G)")
    def PREAMBLE = r("(p|P)(r|R)(e|E)(a|A)(m|M)(b|B)(l|L)(e|E)")
    // can't start with a number, and no quotes, braces/parens, '#', commas, whitespace, or '='
    def SYMBOL = r("[^0-9\"}{)(,\\s#=][^\"}{)(,\\s#=]*")

    object NameLexer {

      import Names._

      def nameLexer = WS ~> ((and | comma | fragment) <~ WS) +
      def and = "and" ^^ (_ => AND)
      def comma = "," ^^ (_ => COMMA)
      def fragment = (braceDelimitedString | "[^\\s,}{]+") ^^ (FRAGMENT(_))

    }

    implicit def c(x: Char): Parser[Char] = accept(x)
    implicit def r(reg: String): Parser[String] = regex(reg.r)
  }
}
