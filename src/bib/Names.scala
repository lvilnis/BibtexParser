package bib

object Names {

  final case class Name(
    first: String,
    von: String,
    last: String,
    jr: String)

  private[bib] sealed trait Token
  private[bib] case object AND extends Token
  private[bib] case object COMMA extends Token
  private[bib] case object HYPHEN extends Token
  private[bib] case class TOKENLIST(ts: List[Token]) extends Token
  private[bib] final case class FRAGMENT(text: String) extends Token

  def stringToNames(names: String): List[Name] =
    fragmentsToNames(lexNameFragments(names))

  private def fragmentsToNames(fragments: List[Token]): List[Name] =
    splitOn(fragments)(AND ==).map(fragmentsToName(_))

  private def fragmentsToName(fragments: List[Token]): Name = {
    val sectionTokens = splitOn(fragments)(COMMA ==)
    val sections = sectionTokens.map(_ map {
      case FRAGMENT(f) => f
      case _ => sys.error("Only fragments should be left over after processing!")
    })
    val isVon: String => Boolean = _.charAt(0).isLower
    sections match {
      case firstVonLast :: Nil if firstVonLast.exists(isVon) =>
        val (first, vonLast) = partitionTakeWhile(firstVonLast)(!isVon(_))
        val (von, last) = partitionTakeWhile(vonLast)(isVon)
        segmentListsToName(first, von, last, Nil)
      case firstLast :: Nil =>
        val (first, last) = partitionTakeRight(firstLast)(1)
        segmentListsToName(first, Nil, last, Nil)
      case vonLast :: first :: Nil =>
        val (von, last) = partitionTakeWhile(vonLast)(isVon)
        segmentListsToName(first, von, last, Nil)
      case vonLast :: jr :: first :: Nil =>
        val (von, last) = partitionTakeWhile(vonLast)(isVon)
        segmentListsToName(first, von, last, jr)
      case _ => sys.error("too many commas in name!")
    }
  }

  def segmentListsToName(
    first: List[String], von: List[String], last: List[String], jr: List[String]) =
    Name(first.mkString(" "), von.mkString(" "), last.mkString(" "), jr.mkString(" "))

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

  private def partitionTakeWhile[T](xs: List[T])(pred: T => Boolean): (List[T], List[T]) =
    (xs.takeWhile(pred), xs.dropWhile(pred))

  private def partitionTakeRight[T](xs: List[T])(toTake: Int): (List[T], List[T]) =
    (xs.dropRight(toTake), xs.takeRight(toTake))

  private def lexNameFragments(namesString: String): List[Names.Token] =
    NameLexer.parseAll(NameLexer.nameLexer, namesString).getOrElse(Nil)

  private def flattenTokenLists(ts: List[Token]): List[Token] = ts flatMap {
    case TOKENLIST(inner) => flattenTokenLists(inner)
    case other => List(other)
  }

  // check out http://www.tug.org/TUGboat/tb27-2/tb87hufflen.pdf
  // I should write a prose description of the rules of how it parses names, as they are extremely complicated
  // TODO: handle hyphens as separators
  object NameLexer extends Parser.BibtexParser {

    def nameLexer =
      WS ~> ((fragment_comma_or_ws | initial) <~ WS).? ~
      ((and_ws | initial | fragment_comma_or_ws) <~ WS).* ~
      (fragment | initial).? ^^ {
        case pre ~ xs ~ post => flattenTokenLists(pre.toList ++ xs ++ post.toList)
      }

    def fragment_comma_or_ws =
      fragment ~ ((WS ~> comma <~ WS) | "\\s+") ^^ {
        case frag ~ COMMA => TOKENLIST(List(frag, COMMA))
        case frag ~ _ => frag
      }

    def and_ws = and <~ "\\s+"
    def and = "and" ^^ (_ => AND)
    def comma = "," ^^ (_ => COMMA)
    def hyphen = ("-" | "~") ^^ (_ => HYPHEN)
    def initial = "[A-Za-z]\\." ^^ (FRAGMENT(_))

    // if its just one fragment with curly braces, its a literal, so leave out the braces
    def fragment =
      (BRACE_DELIMITED_STRING ?) ~ ("[^\\s,}{-~]+" | (BRACE_DELIMITED_STRING ^^ ("{" + _ + "}"))).* ^^ {
        case Some(bds) ~ Nil => bds
        case Some(bds) ~ rest => (("{" + bds + "}") :: rest).mkString
        case None ~ rest => rest.mkString
      } ^^ (FRAGMENT(_))

  }
}
