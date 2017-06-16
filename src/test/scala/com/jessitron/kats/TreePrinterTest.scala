package com.jessitron.kats

import org.scalatest.{FlatSpec, Matchers}

class TreePrinterTest extends FlatSpec with Matchers {

  case class TreeNode(print: String, children: Seq[TreeNode] = Seq())

  val drawTree: TreeNode => String = TreePrinter.draw[TreeNode](_.children, _.print)

  it should "conform to the example" in {
    val expected = """Grandma
├─┬ Dad
| ├── Sister
| └─┬ Me
|   ├── Daughter
|   └── Son
└── Aunt"""

    val input =
      TreeNode("Grandma",Seq(
        TreeNode("Dad",Seq(
            TreeNode("Sister"),
            TreeNode("Me",Seq(
              TreeNode("Daughter"),
              TreeNode("Son"))))),
        TreeNode("Aunt")))

    val output = drawTree(input)

    println(input.toString)

    output should be(expected)

  }
}
