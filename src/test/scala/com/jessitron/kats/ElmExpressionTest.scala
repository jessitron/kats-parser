package com.jessitron.kats

import org.scalatest.FlatSpec

class ElmExpressionTest extends FlatSpec {

  it should "parse this record literal" in {
    val input =
      """            { model
        |                | newLabel = ""
        |                , labels =
        |                    { text = model.newLabel
        |                    , x = 400
        |                    , y = 200
        |                    }
        |                        :: model.labels
        |❡            }""".stripMargin

    val result = ElmParser.parsePart(ElmParser.ElmExpression.recordLiteral, input)

    MyTreeNodePrinting.drawTree(result)

  }
}