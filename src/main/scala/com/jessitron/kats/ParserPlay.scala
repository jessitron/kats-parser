package com.jessitron.kats

object ParserPlay extends App {
  val sourceFile = "/Users/jessitron/code/jessitron/elm-rugs/src/BeginnerProgram.elm"

  val content = io.Source.fromFile(sourceFile).mkString

  val parseTree = ElmParser.parse(ElmProcessor.addSpecials(content))

  println(TreeNodePrinter.drawTree(parseTree))
}