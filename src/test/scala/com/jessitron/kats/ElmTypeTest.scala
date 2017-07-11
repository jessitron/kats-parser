package com.jessitron.kats

import com.atomist.rug.kind.RugLanguageExtensionTest
import org.scalatest.FlatSpec

class ElmTypeTest extends FlatSpec with RugLanguageExtensionTest {


  val sourceProjectLocation = "src/test/resources"

  it should "change my source code" in {

    val pmv = projectFromDirectory(sourceProjectLocation)

    {
      val expr =
        """/BeginnerProgram.elm/Elm()//functionDeclaration
          |             [/functionName[@value="init"]]
          |             //recordLiteralField""".stripMargin

      val initialFieldValues = evaluatePathExpression(pmv, expr)

      val top = evaluatePathExpression(pmv, """/BeginnerProgram.elm/Elm()//functionDeclaration[/functionName[@value="init"]]""")
    //  println(MyTreeNodePrinting.drawUpdatable(top.head))

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

   //   println("New content: ------\n" + newContent + "\n--------")
    }

    {
      val expr =
        """/BeginnerProgram.elm/Elm()//functionDeclaration
          |                          [@functionName='update']/body//caseExpression
          |                              [/pivot[@value='msg']]""".stripMargin

      val v = evaluatePathExpression(pmv, expr)

   //   println(expr + "\n" + MyTreeNodePrinting.drawUpdatable(v.head))

      assert(v.length === 1)
    }

  }

  // Property!
  it should "always break a case clause into a pattern and a result" in {
    val pmv = projectFromDirectory(sourceProjectLocation)

    {
      val expr =
        """//Elm()//caseExpression/clause""".stripMargin

      val v = evaluatePathExpression(pmv, expr)
      v.foreach(clause => {
        assert(clause.childrenNamed("result").length == 1 &&
          clause.childrenNamed("pattern").length == 1,
          "Case clause should have a pattern and a result:\n" + clause.value +
            "\n" + MyTreeNodePrinting.drawUpdatable(clause))
      })
    }
  }

  // Property!
  it should "always break a section into a sectionHeader and sectionContent" in {
    val pmv = projectFromDirectory(sourceProjectLocation)

    {
      val expr =
        """//Elm()//section""".stripMargin

      val v = evaluatePathExpression(pmv, expr)
      v.foreach(clause => {
        assert(clause.childrenNamed("sectionHeader").length == 1 &&
          clause.childrenNamed("sectionContent").length == 1,
          "Section should have a sectionHeader and a sectionContent:\n" + clause.value +
            "\n" + MyTreeNodePrinting.drawUpdatable(clause))
      })
    }
  }

}


