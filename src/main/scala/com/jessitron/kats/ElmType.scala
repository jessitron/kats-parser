package com.jessitron.kats

import com.atomist.rug.kind.grammar.TypeUnderFile
import com.atomist.source.FileArtifact
import com.atomist.tree.content.text.PositionedTreeNode
import com.atomist.tree.content.text.grammar.MatchListener

class ElmType extends TypeUnderFile {
  override def isOfType(f: FileArtifact): Boolean = f.name.endsWith(???)

  override def fileToRawNode(f: FileArtifact, ml: Option[MatchListener]): Option[PositionedTreeNode] = {
       ???
  }

  override def description: String = ???
}
