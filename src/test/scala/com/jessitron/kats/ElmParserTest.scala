package com.jessitron.kats

import java.io.FileNotFoundException

import com.jessitron.kats.ParserPlay.sourceFile
import org.scalatest.FlatSpec

class ElmParserTest extends FlatSpec {

  it should "parse this Elm without an infinite loop" in {

    val contentString = scala.io.Source.fromFile("src/test/resources/TicTacToe.elm").mkString

    ElmParser.parse(ElmProcessor.addSpecials(contentString))
  }

}
