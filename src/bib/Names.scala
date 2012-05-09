package bib

object Names {

  final case class Name(
    first: List[String],
    von: List[String],
    last: List[String],
    jr: List[String])

  private sealed trait Token
  case object AND extends Token
  case object COMMA extends Token
  final case class FRAGMENT(text: String) extends Token

  def stringToNames(names: String): List[Name] =
    fragmentsToNames(lexNameFragments(names))

  private def fragmentsToNames(fragments: List[Token]): List[Name] =
    splitOn(fragments)(AND ==).map(fragmentsToName(_))

  private def fragmentsToName(fragments: List[Token]): Name = {
    val sections = splitOn(fragments)(COMMA ==).map(_.map({ case FRAGMENT(f) => f }))
    if (sections.length > 3) sys.error("too many commas in name!")
    val isVon: String => Boolean = _.charAt(0).isLower
    if (sections.length == 1) {
      // firstVonLast
      val fragments = sections(0)
      val hasVon = fragments.exists(isVon)
      if (!hasVon) {
        val firstFrags = fragments.dropRight(1)
        val last = fragments.drop(1).lastOption
        Name(firstFrags, Nil, last.toList, Nil)
      } else {
        val firstFrags = fragments.takeWhile(!isVon(_))
        val von = fragments.drop(firstFrags.length).takeWhile(isVon)
        val last = fragments.drop(firstFrags.length + von.length)
        Name(firstFrags, von, last, Nil)
      }
    } else if (sections.length == 2) {
      // vonLastFirst
      val vonLast = sections(0)
      val von = vonLast.takeWhile(isVon)
      val last = vonLast.drop(von.length)
      val first = sections(1)
      Name(first, von, last, Nil)
    } else {
      // vonLastJrFirst
      val vonLast = sections(0)
      val von = vonLast.takeWhile(isVon)
      val last = vonLast.drop(von.length)
      val jr = sections(1)
      val first = sections(2)
      Name(first, von, last, jr)
    }
  }

  import annotation.tailrec

  private def splitOn[T](xs: List[T])(pred: T => Boolean): List[List[T]] = {
    @tailrec def loop(
      segments: List[List[T]] = List(Nil),
      remaining: List[T] = xs
      ): List[List[T]] = remaining match {
      case Nil => segments
      case current :: rest =>
        if (pred(current)) loop(Nil :: segments, rest)
        else loop((current :: segments.head) :: segments.tail, rest)
    }
    loop().map(_.reverse).reverse
  }

  private def lexNameFragments(namesString: String): List[Names.Token] =
    NameLexer.parseAll(NameLexer.nameLexer, namesString).getOrElse(Nil)

  object NameLexer extends Parser.BibtexParser {
    def nameLexer = WS ~> ((and | comma | fragment) <~ WS) +
    def and = "and" ^^ (_ => AND)
    def comma = "," ^^ (_ => COMMA)
    def fragment = (BRACE_DELIMITED_STRING | "[^\\s,}{]+") ^^ (FRAGMENT(_))
  }
}
