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
    if (sectionTokens.flatten.length == 0) sys.error("Name must have at least one fragment!")
    val sections = sectionTokens.map(_ map {
      case FRAGMENT(f) => f
      case _ => sys.error("Only fragments should be left over after processing!")
    })
    val isVon: String => Boolean = _.filter(_.isLetter).headOption.map(_.isLower).getOrElse(false)
    sections match {
      case List(firstVonLast) if firstVonLast.exists(isVon) =>
        val (tsal, noVtsrif) = partitionTakeWhile(firstVonLast.reverse, 1)(!isVon(_))
        val (first, von) = partitionTakeWhile(noVtsrif.reverse)(!isVon(_))
        segmentListsToName(first, von, tsal.reverse, Nil)
      case List(firstLast) =>
        val (first, last) = partitionTakeRight(firstLast, 1)
        segmentListsToName(first, Nil, last, Nil)
      case List(vonLast, first) =>
        val (tsal, nov) = partitionTakeWhile(vonLast.reverse, 1)(!isVon(_))
        segmentListsToName(first, nov.reverse, tsal.reverse, Nil)
      case List(vonLast, jr, first) =>
        val (tsal, nov) = partitionTakeWhile(vonLast.reverse, 1)(!isVon(_))
        segmentListsToName(first, nov.reverse, tsal.reverse, jr)
      case _ => sys.error("too many commas in name!")
    }
  }

  private def segmentListsToName(
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

  private def partitionTakeWhile[T](
    xs: List[T], minToTake: Int = 0)(pred: T => Boolean): (List[T], List[T]) = {
    if (minToTake > xs.length) sys.error("minToTake is greater than length of list!")
    val segment = xs.takeWhile(pred)
    val left = if (segment.length < minToTake) xs.take(minToTake) else segment
    (left, xs.drop(math.max(segment.length, minToTake)))
  }

  private def partitionTakeRight[T](xs: List[T], toTake: Int): (List[T], List[T]) =
    (xs.dropRight(toTake), xs.takeRight(toTake))

  private def lexNameFragments(namesString: String): List[Names.Token] =
    NameLexer.parseAll(NameLexer.nameLexer, namesString).getOrElse(Nil)

  private def flattenTokenLists(ts: List[Token]): List[Token] = ts flatMap {
    case TOKENLIST(inner) => flattenTokenLists(inner)
    case other => List(other)
  }

  // check out http://www.tug.org/TUGboat/tb27-2/tb87hufflen.pdf
  // I should write a prose description of the rules of how it parses names, as they are extremely complicated
  // FIXME: should hyphens make it to the later phase? what makes them different from plain old spaces?
  object NameLexer extends Parser.BibtexParser {

    def nameLexer =
      WS ~> ((initial | fragment_comma_or_ws) <~ WS).? ~
      ((and_ws | initial | hyphen | fragment_comma_or_ws) <~ WS).* ~
      (fragment | initial).? ^^ {
        case pre ~ xs ~ post => flattenTokenLists(pre.toList ++ xs ++ post.toList).filterNot(HYPHEN ==)
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
      (BRACE_DELIMITED_STRING_NO_OUTER ?) ~ ("[^\\s,}{-~]+" | BRACE_DELIMITED_STRING).* ^^ {
        case Some(bds) ~ Nil => bds
        case Some(bds) ~ rest => (("{" + bds + "}") :: rest).mkString
        case None ~ rest => rest.mkString
      } ^^ (FRAGMENT(_))

  }
}
