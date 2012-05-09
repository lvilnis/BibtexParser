package bib

object Dom {

  final case class Document(
    comments: List[String],
    preambles: List[String],
    entries: Map[String, Entry])

  final case class Entry(
    ty: String,
    citationKey: String,
    crossReference: Option[Entry],
    authors: List[Names.Name],
    editors: List[Names.Name],
    otherFields: Map[String, String])

  import annotation.tailrec

  def stringToDom(str: String): Option[Document] = Parser.parseString(str).map(astToDom(_))

  def astToDom(astDoc: AST.Document): Document = {

    // NOTE: could make this a map with a default entry that passes things through unchanged
    // if we want it to give something useful for missing stuff?
    val standardEnvironment =
      Map("jan" -> "jan", "feb" -> "feb", "mar" -> "mar", "apr" -> "apr",
        "may" -> "may", "jun" -> "jun", "jul" -> "jul", "aug" -> "aug", "sep" -> "sep",
        "oct" -> "oct", "nov" -> "nov", "dec" -> "dec")

    val emptyDocument = Document(Nil, Nil, Map.empty)

    def evalValue(value: AST.Value, env: Map[String, String]): String = value match {
      case AST.Literal(str) => str
      case AST.Abbrev(id) => env(id)
      case AST.Concat(l, r) => evalValue(l, env) + evalValue(r, env)
    }
    // can crossref reference entries that haven't been created yet?
    // if so, we need to topological sort first (or do 2 passes)
    @tailrec def loop(
      currentDoc: Document = emptyDocument,
      astEntries: List[AST.Entry] = astDoc.entries,
      env: Map[String, String] = standardEnvironment
      ): Document = astEntries match {
      case Nil => currentDoc
      case entry :: rest => entry match {

        case AST.StringEntry(name, value) =>
          loop(currentDoc, rest, env + (name -> evalValue(value, env)))

        case AST.CommentEntry(comment) =>
          val newComments =
            if (comment.trim.isEmpty) currentDoc.comments
            else comment :: currentDoc.comments
          loop(currentDoc.copy(comments = newComments), rest, env)

        case AST.PreambleEntry(pre) =>
          loop(currentDoc.copy(preambles = evalValue(pre, env) :: currentDoc.preambles), rest, env)

        case AST.RegularEntry(ty, citationKey, tags) =>
          val evaldTags = tags.toMap.mapValues(evalValue(_, env))
          // FIXME: should "crossref"/"author"/"editor" all be case-insensitive?
          val crossRefEntry = for {
            referenceName <- evaldTags.get("crossref")
            referenceEntry <- currentDoc.entries.get(referenceName)
          } yield referenceEntry
          def namesForField(fieldName: String) =
            evaldTags.get(fieldName).map(Names.stringToNames(_)).toList.flatten
          val remainingTags = evaldTags - "crossref" - "author" - "editor"
          val entry = Entry(
            ty, citationKey, crossRefEntry, namesForField("author"), namesForField("editor"), remainingTags)
          loop(currentDoc.copy(entries = currentDoc.entries + (entry.citationKey -> entry)), rest, env)
      }
    }
    loop()
  }

}
