package com.jessitron.kats

import com.atomist.tree.UpdatableTreeNode
import com.jessitron.kats.TreePrinter.draw

object MyTreeNodePrinting {

  def drawTree: PositionedSyntaxNode => String = draw[PositionedSyntaxNode](_.parsedNodes,
    n=>  s"[${n.startOffset}-${n.endOffset}] ${n.nodeName}" + n.valueOption.map(" is " + _).getOrElse(""))

  def drawUpdatable: UpdatableTreeNode => String = draw[UpdatableTreeNode](_.childNodes.collect{ case u: UpdatableTreeNode => u},
    n=>  if (n.childNodes.isEmpty) s"${n.nodeName} is " + n.value else n.nodeName)

}
