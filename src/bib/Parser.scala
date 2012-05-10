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

  // this should return an Either or a custom error object instead of a useless "None"
  def parseString(input: String): Option[Document] = {
    val res = ParserImpl.parseAll(ParserImpl.bibTex, input)
    res.map(r => Some(Document(r))).getOrElse(None)
  }

  import scala.util.parsing.combinator._

  trait BibtexParser extends RegexParsers {
    override val skipWhitespace = false
    // FIXME: this should be '+' not '*' - go find places that rely on it being '+' and add a '?'
    def WS = r("\\s*")
    def BRACE_DELIMITED_STRING: Parser[String] =
      '{' ~> (BRACE_DELIMITED_STRING ^^ ("{" + _ + "}") | """\\.""" | """[^}{]""").* <~ '}' ^^
      (_.mkString)

    implicit def c(x: Char): Parser[Char] = accept(x)
    implicit def r(reg: String): Parser[String] = regex(reg.r)
  }

  private[bib] object ParserImpl extends BibtexParser {

    def bibTex =
      (freeComment ~! anyEntry ~! freeComment).+ ^^
      (_ flatMap { case x ~ y ~ z => List(x, y, z): List[Entry] })

    // fixme: lines starting with %%% are comments
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
    def quoteDelimitedStringLiteral =
      '"' ~> (BRACE_DELIMITED_STRING | """[^"]""" | """\\.""").* <~ '"' ^^ (xs => Literal(xs.mkString))
    def braceDelimitedStringLiteral = BRACE_DELIMITED_STRING ^^ (Literal(_))

    def AT = c('@')
    def COMMA_WS = r("\\s*,\\s*")
    def COMMENT = r("(c|C)(o|O)(m|M)(m|M)(e|E)(n|N)(t|T)")
    def STRING = r("(s|S)(t|T)(r|R)(i|I)(n|N)(g|G)")
    def PREAMBLE = r("(p|P)(r|R)(e|E)(a|A)(m|M)(b|B)(l|L)(e|E)")
    // can't start with a number, and no quotes, braces/parens, '#', commas, whitespace, or '='
    def SYMBOL = r("[^0-9\"}{)(,\\s#=][^\"}{)(,\\s#=]*")
  }
}
