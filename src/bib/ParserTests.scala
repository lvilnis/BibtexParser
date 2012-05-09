package bib

object ParserTests {

  import Parser.ParserImpl

  def main(args: Array[String]): Unit = {

    // I know, I know - these aren't real unit tests. Soon!
    println(ParserImpl.parseAll(ParserImpl.braceDelimitedStringLiteral, "{Something Great}"))
    println(ParserImpl.parseAll(ParserImpl.literal, "{Something Great}"))
    println(ParserImpl.parseAll(ParserImpl.literalOrSymbol, "{Something Great}"))
    println(ParserImpl.parseAll(ParserImpl.value, "{Something Great}"))

    println(ParserImpl.parseAll(ParserImpl.quoteDelimitedStringLiteral, "\"Something Great\""))
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
      ParserImpl.braceDelimitedStringLiteral,
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
  author =       {K.S.Narendra and K.Parthsarathy},
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

    println(Names.NameLexer.parseAll(Names.NameLexer.fragmentFollowedByCommaOrWhitespace+, "Bethe, Hans "))

    // here's a really tricky one (not a french word, i know)
    println(Names.stringToNames("{\\e'}col{\\e'}"))
  }
}
