package com.jessitron.kats

import com.atomist.rug.kind.grammar.{ParsedNode, TypeUnderFile}
import com.atomist.source.FileArtifact

import scala.util.control.NonFatal

class ElmType extends TypeUnderFile {
  override def isOfType(f: FileArtifact): Boolean = f.name.endsWith(".elm")

  override def fileToRawNode(f: FileArtifact): Option[ParsedNode] = {
    println(s"Attempting to parse ${f.path}")
    try {
      val result = ElmParser.parse(f.content)
      println(s"~~~~~~ Successfully parsed ${f.path}")
      Some(result)
    } catch {
      case NonFatal(x) => throw new RuntimeException(s"Failure parsing file: ${f.path}", x)
    }
  }

  override def description: String = "elmy elminess"

  override def preprocess(originalContent: String): String = {
   val result = markMovesToTheLeft(originalContent.replaceAll("(?m)^(\\S)", "\\☞$1"))
    println("PREPOCESS OUTPUT\n" + result)
    result
  }

  // strip the preprocess marks
  override def postprocess(preprocessedContent: String): String =
    preprocessedContent.replaceAll("[☞❡]", "")


  private def markMovesToTheLeft(originalContent: String): String =
  {
    val firstLine = originalContent.lines.next()
    val otherlines = originalContent.lines.sliding(2, 1).map {
      it =>
        val prev = it.head
        val curr = it.last
        val prevSpaces = prev.indexOf(prev.trim())
        val currentSpaces = curr.indexOf(curr.trim())
        if (0 < currentSpaces && currentSpaces < prevSpaces)
          "❡" + curr
        else
          curr
    }.toSeq
    (firstLine +: otherlines).mkString("\n")
  }

}
