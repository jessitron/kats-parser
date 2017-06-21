package com.jessitron.kats

object ParserPlay extends App {
  val sourceFile = "src/test/resources/Program.elm"

  val content = io.Source.fromFile(sourceFile).mkString

  val parseTree = ElmParser.parse(ElmProcessor.addSpecials(content))


  println(MyTreeNodePrinting.drawTree(parseTree))

}