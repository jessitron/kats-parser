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
    val lines = originalContent.lines.foldLeft[(Int, Seq[String])]((0, Seq())) {
      case ((prevIndent, lines), current) =>
        if (current.trim().isEmpty)
          (prevIndent, lines :+ current)
        else {
          val currentIndent = current.indexOf(current.trim())
          val thisLine = if (0 < currentIndent && currentIndent < prevIndent) "❡" + current else current
          (currentIndent, lines :+ thisLine)
        }
    }
    lines._2.mkString("\n")
  }

}
