package com.jessitron.kats

import com.atomist.rug.kind.grammar.{ParsedNode, TypeUnderFile}
import com.atomist.source.FileArtifact

import scala.util.control.NonFatal

class ElmType extends TypeUnderFile {
  override def isOfType(f: FileArtifact): Boolean = f.name.endsWith(".elm")

  override def fileToRawNode(f: FileArtifact): Option[ParsedNode] = {
    try {
      Some(ElmParser.parse(f.content))
    } catch {
      case NonFatal(x) => throw new RuntimeException(s"Failure parsing file: ${f.path}", x)
    }
  }

  override def description: String = "elmy elminess"
}
