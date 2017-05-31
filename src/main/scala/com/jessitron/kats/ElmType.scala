package com.jessitron.kats

import com.atomist.rug.kind.grammar.{ParsedNode, TypeUnderFile}
import com.atomist.source.FileArtifact

class ElmType extends TypeUnderFile {
  override def isOfType(f: FileArtifact): Boolean = f.name.endsWith(".elm")

  override def fileToRawNode(f: FileArtifact): Option[ParsedNode] = {
       Some(ElmParser.parse(f.content))
  }

  override def description: String = "elmy elminess"
}
