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
    authors: Option[List[Names.Name]],
    editors: Option[List[Names.Name]],
    otherFields: Map[String, String])

  import annotation.tailrec

  def stringToDom(str: String, expandAbbreviations: Boolean = true): Option[Document] =
    Parser.parseString(str).map(astToDom(_, expandAbbreviations))

  def astToDom(astDoc: AST.Document, expandAbbreviations: Boolean = true): Document = {

    // NOTE: map has a default entry that passes things through unchanged
    // many fields (acknowledgement, etc) don't quote their string inputs so we should just pass thru
    // month abbreviations also are not really useful to expand
    val standardEnvironment = Map.empty[String, String].withDefault(identity)

    val emptyDocument = Document(Nil, Nil, Map.empty)

    def evalValue(value: AST.Value, env: Map[String, String]): String = value match {
      case AST.Literal(str) => str
      case AST.Abbrev(id) => if (expandAbbreviations) env(id) else id
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
          val authorNames = Some(namesForField("author"))
          val editorNames = Some(namesForField("editor"))
          val entry = Entry(ty, citationKey, crossRefEntry, authorNames, editorNames, remainingTags)
          loop(currentDoc.copy(entries = currentDoc.entries + (entry.citationKey -> entry)), rest, env)
      }
    }
    loop()
  }

}
