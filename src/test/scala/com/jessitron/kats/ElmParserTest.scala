package com.jessitron.kats

import java.util.Timer

import com.jessitron.kats.ParserPlay.sourceFile
import org.scalatest.FlatSpec

class ElmParserTest extends FlatSpec {

//  it should "parse this Elm without an infinite loop" in {
//
//    val contentString = scala.io.Source.fromFile("src/test/resources/TicTacToe.elm").mkString
//
//    ElmParser.parse(ElmProcessor.addSpecials(contentString))
//  }

  def ns(qty: Int):String = {
    Stream.continually("  N").take(qty).mkString("\n")
  }
  it should "not be horribly slow" in {
    val content = """TicTacToe""".stripMargin

    for (i <- Range(1,8)) {
      val (t, _) = time(
      ElmParser.parsePart(ElmParser.ElmExpression.expression("some Type parameters"), content + ns(i) + "\n"))
      println(s"$i ${(t.toFloat) / (1000 * 1000 * 1000)}")
    }
  }

  def time[R](block: => R): (Long, R) = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    (t1 - t0, result)
  }

}
