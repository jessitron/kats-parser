package com.jessitron.kats

object ParserPlay extends App {
  val sourceFile = "/Users/jessitron/code/elm/Kats/src/Main.elm"

  val content = io.Source.fromFile(sourceFile).mkString

  val parseTree = ElmParser.parse(content)

  println(parseTree)
}