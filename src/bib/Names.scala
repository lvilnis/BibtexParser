package bib

object Names {

  final case class Name(
    first: List[String],
    von: List[String],
    last: List[String],
    jr: List[String])

  private sealed trait Token
  private case object AND extends Token
  private case object COMMA extends Token
  private final case class FRAGMENT(text: String) extends Token

  def stringToNames(names: String): List[Name] =
    fragmentsToNames(lexNameFragments(names))

  private def fragmentsToNames(fragments: List[Token]): List[Name] =
    splitOn(fragments)(AND ==).map(fragmentsToName(_))

  private def fragmentsToName(fragments: List[Token]): Name = {
    val sections = splitOn(fragments)(COMMA ==).map(_.map({ case FRAGMENT(f) => f }))
    val isVon: String => Boolean = _.charAt(0).isLower
    sections match {
      case firstVonLast :: Nil if firstVonLast.exists(isVon) =>
        val (first, vonLast) = partitionTakeWhile(firstVonLast)(!isVon(_))
        val (von, last) = partitionTakeWhile(vonLast)(isVon)
        Name(first, von, last, Nil)
      case firstLast :: Nil =>
        val (first, last) = partitionTakeRight(firstLast)(1)
        Name(first, Nil, last, Nil)
      case vonLast :: first :: Nil =>
        val (von, last) = partitionTakeWhile(vonLast)(isVon)
        Name(first, von, last, Nil)
      case vonLast :: jr :: first :: Nil =>
        val (von, last) = partitionTakeWhile(vonLast)(isVon)
        Name(first, von, last, jr)
      case _ => sys.error("too many commas in name!")
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

  private def partitionTakeWhile[T](xs: List[T])(pred: T => Boolean): (List[T], List[T]) =
    (xs.takeWhile(pred), xs.dropWhile(pred))

  private def partitionTakeRight[T](xs: List[T])(toTake: Int): (List[T], List[T]) =
    (xs.dropRight(toTake), xs.takeRight(toTake))

  private def lexNameFragments(namesString: String): List[Names.Token] =
    NameLexer.parseAll(NameLexer.nameLexer, namesString).getOrElse(Nil)

  private object NameLexer extends Parser.BibtexParser {
    def nameLexer = WS ~> ((and | comma | fragment) <~ WS) +
    def and = "and" ^^ (_ => AND)
    def comma = "," ^^ (_ => COMMA)
    def fragment = (BRACE_DELIMITED_STRING | "[^\\s,}{]+") ^^ (FRAGMENT(_))
  }
}
