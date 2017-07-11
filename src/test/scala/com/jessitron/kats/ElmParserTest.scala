package com.jessitron.kats


import org.scalatest.FlatSpec

class ElmParserTest extends FlatSpec {

  it should "parse this Elm" in {
    /* There's a story here: At the STL Polyglots meeting (7/17) we did some
     * mob programming in Elm and we really needed this upgrade program
     * but it didn't work, it just ran forever.
     *
     * Later it turns out, that was a pathological performance case.
     * Fixed by being specific that arguments to a constructor are expressionsWithClearPrecedence.
     */

    val contentString = scala.io.Source.fromFile("src/test/resources/TicTacToe.elm").mkString

    ElmParser.parse(ElmProcessor.addSpecials(contentString))
  }


  it should "not be horribly slow parsing a constructor with many arguments" in {
    val content = """TicTacToe""".stripMargin

    def ns(qty: Int):String = {
      Stream.continually("  N").take(qty).mkString("\n")
    }

    for (i <- Range(1,10)) {
      val (t, _) = time(
      ElmParser.parsePart(ElmParser.ElmExpression.constructorApplication, content + ns(i) + "\n"))
    //  println(s"$i ${(t.toFloat) / (1000 * 1000 * 1000)}")
    }
  }

  def time[R](block: => R): (Long, R) = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    (t1 - t0, result)
  }

}
