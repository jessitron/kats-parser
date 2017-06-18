package com.jessitron.kats

import com.atomist.rug.kind.grammar.{ParsedNode, TypeUnderFile}
import com.atomist.source.FileArtifact

import scala.util.control.NonFatal

class ElmType extends TypeUnderFile {

  override def isOfType(f: FileArtifact): Boolean =
    f.name.endsWith(".elm") && !f.path.contains("elm-stuff/")

  override def fileToRawNode(f: FileArtifact): Option[ParsedNode] = {
    if (System.getProperty("ELM_TYPE_DEBUG") != null) {
      // this is a cheaty way to config but it's simple
      println(s"Parsing ${f.path} with Elm Parser version ${ElmParser.VERSION}")
    }
    //println(s"Attempting to parse ${f.path}")
    try {
      val result = ElmParser.parse(f.content)
      //println(s"~~~~~~ Successfully parsed ${f.path}")
      Some(result)
    } catch {
      case NonFatal(x) =>
        println("contents that didn't parse: -----\n" + f.content + "\n-------")
        throw new RuntimeException(s"Failure parsing file: ${f.path}", x)
    }
  }

  override def description: String = "elmy elminess"

  override def preprocess(originalContent: String): String = {
   ElmProcessor.addSpecials(originalContent)
  }

  override def postprocess(preprocessedContent: String): String =
    ElmProcessor.removeSpecials(preprocessedContent)


}
