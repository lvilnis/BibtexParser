package bib

private[bib] sealed trait ASTNode

sealed trait Entry extends ASTNode
sealed trait Value extends ASTNode

case class Document(entries: List[Entry]) extends ASTNode

case class StringEntry(abbrev: String, value: Value) extends Entry
case class PreambleEntry(content: Value) extends Entry
case class CommentEntry(comment: String) extends Entry
case class RegularEntry(ty: String, citationKey: String, tags: List[(String, Value)]) extends Entry

case class Literal(content: String) extends Value
case class Abbrev(name: String) extends Value
case class Concat(left: Value, right: Value) extends Value

object Parser {

  def parseString(input: String): Option[Document] = {
    val res = ParserImpl.parseAll(ParserImpl.bibTex, input)
    res.map(r => Some(Document(r))).getOrElse(None)
  }

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

    def preambleEntry = PREAMBLE ~>  WS ~> entryBody { value } ^^ (PreambleEntry(_))

    def regularEntry =
      (SYMBOL <~  WS) ~ entryBody { SYMBOL ~ rep(COMMA_WS ~> tag) <~ (COMMA_WS ?) } ^^ {
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
    def braceDelimitedString: Parser[String] =
      '{' ~> (((braceDelimitedString ^^ ("{" + _ + "}")) | "[^}{]+").* ^^ (_.fold("")(_ + _))) <~ '}'
    def braceDelimitedStringLiteral = braceDelimitedString ^^ (Literal(_))
    def quoteDelimitedStringLiteral = '"' ~> "[^\"]*" <~ '"' ^^ (Literal(_))

    def AT = c('@')
    def WS = r("\\s*")
    def COMMA_WS = r("\\s*,\\s*")
    def COMMENT = r("(c|C)(o|O)(m|M)(m|M)(e|E)(n|N)(t|T)")
    def STRING = r("(s|S)(t|T)(r|R)(i|I)(n|N)(g|G)")
    def PREAMBLE = r("(p|P)(r|R)(e|E)(a|A)(m|M)(b|B)(l|L)(e|E)")
    def SYMBOL = r("[A-Za-z][A-Za-z0-9_:]*") // FIXME: figure out what other characters are allowed in symbols

    implicit def c(x: Char): Parser[Char] = accept(x)
    implicit def r(reg: String): Parser[String] = regex(reg.r)
  }
}
