package com.jessitron.kats

import com.atomist.rug.kind.grammar.TypeUnderFile
import com.atomist.source.FileArtifact
import com.atomist.tree.content.text.PositionedTreeNode
import com.atomist.tree.content.text.grammar.MatchListener

class ElmType extends TypeUnderFile {
  override def isOfType(f: FileArtifact): Boolean = f.name.endsWith(".elm")

  override def fileToRawNode(f: FileArtifact, ml: Option[MatchListener]): Option[PositionedTreeNode] = {
       Some(ElmParser.parse(f.content))
  }

  override def description: String = "elmy elminess"
}
