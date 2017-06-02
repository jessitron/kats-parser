package com.jessitron.kats

object ParserPlay extends App {
  val sourceFile = "/Users/jessitron/code/jessitron/elm-rugs/src/BeginnerProgram.elm"

  val content = io.Source.fromFile(sourceFile).mkString

  val parseTree = ElmParser.parse(ElmProcessor.addSpecials(content))


  println(TreeNodePrinter.drawTree(parseTree))

  val typeRef = parseTree.parsedNodes(2).parsedNodes(0).parsedNodes(1)
  println(TreeNodePrinter.drawTree(parseTree.parsedNodes(2).parsedNodes(0).parsedNodes(1)))
  println(typeRef)
}