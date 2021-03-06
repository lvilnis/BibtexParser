package bib

object ParserTests {

  import Parser.ParserImpl

  // check out this site for test data: http://ftp.math.utah.edu/pub/bibnet/subjects/
  // (BibNet is public domain)
  def main(args: Array[String]): Unit = {

    // I know, I know - these aren't real unit tests. Soon!
    println(ParserImpl.parseAll(ParserImpl.braceDelimitedNoOuterLiteral, "{Something Great}"))
    println(ParserImpl.parseAll(ParserImpl.literal, "{Something Great}"))
    println(ParserImpl.parseAll(ParserImpl.literalOrSymbol, "{Something Great}"))
    println(ParserImpl.parseAll(ParserImpl.value, "{Something Great}"))

    println(ParserImpl.parseAll(ParserImpl.quoteDelimitedLiteral, "\"Something Great\""))
    println(ParserImpl.parseAll(ParserImpl.literal, "\"Something Great\""))
    println(ParserImpl.parseAll(ParserImpl.literalOrSymbol, "\"Something Great\""))
    println(ParserImpl.parseAll(ParserImpl.value, "\"Something Great\""))

    println(ParserImpl.parseAll(ParserImpl.numericLiteral, "123"))
    println(ParserImpl.parseAll(ParserImpl.literal, "123"))
    println(ParserImpl.parseAll(ParserImpl.literalOrSymbol, "123"))
    println(ParserImpl.parseAll(ParserImpl.value, "123"))

    println(ParserImpl.parseAll(ParserImpl.SYMBOL, "asda5"))
    println(ParserImpl.parseAll(ParserImpl.literalOrSymbol, "asda5"))
    println(ParserImpl.parseAll(ParserImpl.value, "asda5"))

    println(ParserImpl.parseAll(ParserImpl.tag, "asda5 = { 132 as qwe  asd }"))

    println(ParserImpl.parseAll(ParserImpl.value, "asda5 # asda5"))

    println(ParserImpl.parseAll(ParserImpl.commentEntry, "comment{wooooo!}"))

    println(ParserImpl.parseAll(ParserImpl.preambleEntry, "preamble{wooooo}"))

    println(ParserImpl.parseAll(ParserImpl.stringEntry, "string{wooooo = 1231}"))
    println(ParserImpl.parseAll(ParserImpl.anyEntry, "@string{wooooo = 1231}"))
    println(ParserImpl.parseAll(ParserImpl.anyEntry, "@string{  wooooo  = {asd} }"))

    println(ParserImpl.parseAll(ParserImpl.anyEntry, "@string{  wooooo  = {asd} }"))
    println(ParserImpl.parseAll(ParserImpl.anyEntry, "@preamble{  wooooo}"))
    println(ParserImpl.parseAll(ParserImpl.anyEntry, "@comment{  wooooo }"))

    println(ParserImpl.parseAll(ParserImpl.anyEntry, "@florb{  wooooo }"))
    println(ParserImpl.parseAll(ParserImpl.anyEntry, "@florb{  wooooo, x = {y}, fg = sdf13, z = 123 }"))
    println(ParserImpl.parseAll(ParserImpl.anyEntry, "@florb{  wooooo, x = {y}, fg = sdf13, z = 123, }"))
    println(ParserImpl.parseAll(ParserImpl.anyEntry, "@florb{  wooooo, x = {y}, fg =\"sdf13\", z = 123, }"))
    println(ParserImpl.parseAll(ParserImpl.anyEntry,
      """@florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }"""))

    println(ParserImpl.parseAll(ParserImpl.freeComment, "i am the king of the owrld!!"))
    println(ParserImpl.parseAll(ParserImpl.freeComment, """i am the king of the

    owrld!!"""))

    println(ParserImpl.parseAll(ParserImpl.WS ~> ParserImpl.anyEntry,
      """ @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }"""))

    println(ParserImpl.parseAll((ParserImpl.WS ~> ParserImpl.anyEntry) +,
      """ @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }"""))

    println(ParserImpl.parseAll(ParserImpl.bibTex,
      """ @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }"""))

    println(ParserImpl.parseAll(ParserImpl.bibTex,
      """ @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }

      """
    ))

    println(ParserImpl.parseAll(ParserImpl.bibTex,
      """
       Hi, everybody!

       @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }
 @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }
 @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }

 free comments are coool
 @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }


      """))


    println(ParserImpl.parseAll(ParserImpl.bibTex,
      """
          @article {mrx05,
          auTHor = "Mr. X",
          Title = {Something Great},
          publisher = "nob" # "ody",
          YEAR = 2005
          }
      """))

    println(ParserImpl.parseAll(
      ParserImpl.braceDelimitedNoOuterLiteral,
      "{Interannual Variability of planet-encircling dust activity on {M}ars}"))

    // this sample is from: http://amath.colorado.edu/documentation/LaTeX/reference/faq/bibstyles.html
    val coloradoSample = Parser.parseString(
      """

@string{jgr = "J.~Geophys.~Res."}

@MISC{primes,
   author = "Charles Louis Xavier Joseph de la Vall{\'e}e Poussin",
   note = "A strong form of the prime number theorem, 19th century",
   year = 1879
   }

@INBOOK{chicago,
   title = "The Chicago Manual of Style",
   publisher = "University of Chicago Press",
   edition = "Thirteenth",
   year = 1982,
   pages = "400--401",
   key = "Chicago"
   }

@BOOK{texbook,
   author = "Donald E. Knuth",
   title= "The {{\TeX}book}",
   publisher = "Addison-Wesley",
   year = 1984
   }

@BOOK{latexbook,
   author = "Leslie Lamport",
   title = "{\LaTeX \rm:} {A} Document Preparation System",
   publisher = "Addison-Wesley",
   year = 1986
   }

@UNPUBLISHED{btxdoc,
   author = "Oren Patashnik",
   title = "{Using BibTeX}",
   note = "Documentation for general BibTeX users",
   month = jan,
   year = 1988
   }

@UNPUBLISHED{btxhak,
   author = "Oren Patashnik",
   title = "Designing BibTeX Styles",
   note = "The part of BibTeX's documentation
                            that's not meant for general users",
   month = jan,
   year = 1988
   }

@BOOK{strunk,
   author = "Strunk, Jr., William and E. B. White",
   title = "The Elements of Style",
   publisher = "Macmillan",
   edition = "Third",
   year = 1979
   }

@book{vanleunen,
   title = "A Handbook for Scholars",
   author = "Mary-Claire van Leunen",
   publisher = "Knopf",
   year = "1979"
   }

@ARTICLE{Zurek:1993,
   AUTHOR  = {Zurek, R. W. and Martin, L. J.},
   TITLE   = {Interannual Variability of planet-encircling dust activity on {M}ars},
   YEAR    = {1993},
   JOURNAL = jgr,
   VOLUME  = {98},
   NUMBER  = {E2},
   PAGES   = {3247--3259}
}

@Article{Narendra_1990,
  author =       {K.S.Narendra and K.S.Parthsarathy},
  title =        {Identification and Control of Dynamical System
                  using Neural Networks},
  journal =      "IEENN",
  year =         {1990},
  volume =    {1},
  number =    {1},
  month =     {},
  pages =     {4-27},
  note =      {},
  annote =    {}
}


      """)
    println(coloradoSample)
    println(Dom.astToDom(coloradoSample.get))

    println(Names.stringToNames("Ludwig von Beethoven"))
    println(Names.stringToNames("von Beethoven, Ludwig"))
    println(Names.stringToNames("Jones, John Paul"))
    println(Names.stringToNames("John Paul Jones"))

    println(Names.stringToNames("John Paul Jones and Jones, John Paul"))
    println(Names.stringToNames("John Paul Jones and Ludwig von Beethoven"))

    println(Names.stringToNames("Charles Louis Xavier Joseph de la Vallee Poussin"))

    println(Names.stringToNames("{Barnes} {and} {Noble,} {Inc.}"))

    println(Names.stringToNames("Ralph Alpher and Bethe, Hans and George Gamow"))
    println(Names.stringToNames("K.S.Narendra"))

    println(Names.stringToNames("{\\e'}cole"))

    println(Names.stringToNames("John-Paul Jones and Bill Thompson"))

    println(Names.NameLexer.parseAll(Names.NameLexer.fragment_comma_or_ws +, "Bethe, Hans "))

    // here's a really tricky one (not a french word, i know)
    println(Names.stringToNames("{\\e'}col{\\e'}"))

    println(Names.NameLexer.parseAll(Names.NameLexer.fragment, "{\\e'}col{\\e'}"))

    println(Names.stringToNames("{hey ho lotsa stu\\}ff}"))
    println(Names.stringToNames("{Jean} {de la Fontaine du} {Bois Joli}"))
    println(Names.stringToNames("Jean de la Fontaine du Bois Joli"))


    val clx1 = Names.stringToNames("Charles Louis Xavier Joseph de la Vall{\\'e}e Poussin").head
    println(clx1)
    val clx2 = Dom.stringToDom("@thing{asdf, author = \"Charles Louis Xavier Joseph de la Vall{\\'e}e Poussin\"}")
      .get.entries.head._2.authors.get.head
    println(clx2)
    val clx3 = Dom.stringToDom("@thing{asdf, author = {Charles Louis Xavier Joseph de la Vall{\\'e}e Poussin}}")
      .get.entries.head._2.authors.get.head
    println(clx3)

    println(clx1 == clx2 && clx2 == clx3)

    val ksn1 = Names.stringToNames("K.S.Narendra").head
    println(ksn1)
    val ksn2 = Dom.stringToDom("@thing{asdf, author = \"K.S.Narendra\"}")
      .get.entries.head._2.authors.get.head
    println(ksn2)
    val ksn3 = Dom.stringToDom("@thing{asdf, author = {K.S.Narendra}}")
      .get.entries.head._2.authors.get.head
    println(ksn3)
    val ksn4 = Dom.stringToDom("@thing{asdf, author = {K.S.Narendra and Hugh Jass}}")
      .get.entries.head._2.authors.get.head
    println(ksn4)

    println(ksn1 == ksn2 && ksn2 == ksn3 && ksn3 == ksn4)

    val fileText = scala.io.Source.fromFile("inputs/case-based-reasoning.bib.txt").mkString
    val res = Dom.stringToDom(fileText, false)
    //println(res)

    def timed[T](showTime: Long => String)(body: => T) = {
      val start = System.currentTimeMillis
      val result = body
      val time = showTime(System.currentTimeMillis - start)
      println(time)
      (result, time)
    }

    val filePath2 = "inputs/domain-decomp.bib.txt"
    val file2 = scala.io.Source.fromFile(filePath2).toArray
    val fileText2 = file2.mkString

    val numLines = file2.length
    val numMb = new java.io.File(filePath2).length / 1024.0 / 1024.0

    val (result, time) =
      timed(t =>
        "domain-decomp.bib (%f MB, %d lines) parsed and dom-ified in %d ms (%f MB/sec, %f lines/sec)" format
        (numMb, numLines, t, (1000.0 * numMb) / t, (1000.0 * numLines) / t)) {
        Dom.stringToDom(fileText2, false)
      }

    //    println(result)
    println(time)
    val sizeMult = 10
    val bigtext = List.range(0, sizeMult).map(_ => fileText2).mkString
    val (bigresult, bigtime) =
      timed(t =>
        "%d times domain-decomp.bib (%f MB, %d lines) parsed and dom-ified in %d ms (%f MB/sec, %f lines/sec)" format
        (sizeMult, numMb * sizeMult, numLines * sizeMult, t, (1000.0 * numMb * sizeMult) / t, (1000.0 * numLines * sizeMult) / t)) {
        Dom.stringToDom(bigtext, false)
      }
  }
}
