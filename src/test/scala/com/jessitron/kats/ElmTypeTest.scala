package com.jessitron.kats

import com.atomist.rug.kind.RugLanguageExtensionTest
import org.scalatest.FlatSpec

class ElmTypeTest extends FlatSpec with RugLanguageExtensionTest {

  it should "change my source code" in {
    val sourceProjectLocation = "src/test/resources"

    val pmv = projectFromDirectory(sourceProjectLocation)



    {
      val expr =
        """/BeginnerProgram.elm/Elm()//functionDeclaration
          |             [/functionName[@value="init"]]
          |             //recordLiteralField""".stripMargin

      val initialFieldValues = evaluatePathExpression(pmv, expr)

      val top = evaluatePathExpression(pmv, """/BeginnerProgram.elm/Elm()//functionDeclaration[/functionName[@value="init"]]""")
      println(MyTreeNodePrinting.drawUpdatable(top.head))

      assert(initialFieldValues.length === 2)
    }

    {
      val expr =
        """/BeginnerProgram.elm/Elm()//functionDeclaration
          |             [/functionName[@value="view"]]
          |             /body/functionApplication[//function[@value="Html.div"]]
          |                  /argument[2]""".stripMargin

      val nodes = evaluatePathExpression(pmv, expr)

      nodes.head.update("[ Html.text \"Hello World\" ]")

      val newContent = pmv.findFile("BeginnerProgram.elm").content
      assert(newContent.contains("Hello World"))

      println("New content: ------\n" + newContent + "\n--------")
    }

    { val expr =
      """/BeginnerProgram.elm/Elm()//functionDeclaration
        |                          [@functionName='update']/body//caseExpression
        |                              [/pivot[@value='msg']]""".stripMargin

      val v = evaluatePathExpression(pmv, expr)

      println(expr + "\n" + MyTreeNodePrinting.drawUpdatable(v.head))

      assert(v.length === 1)
    }

  }

}
