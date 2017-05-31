package com.jessitron.kats

import com.atomist.rug.kind.RugLanguageExtensionTest
import org.scalatest.FlatSpec

class ElmTypeTest extends FlatSpec with RugLanguageExtensionTest {

  it should "change my source code" in {
    val sourceProjectLocation = "/Users/jessitron/code/jessitron/elm-rugs/elm-stuff"

    val pmv = projectFromDirectory(sourceProjectLocation)

    val expr =
      """//Elm()//functionDeclaration
        |             [/functionName[@value="view"]]
        |             /body/functionApplication[//calledFunction[@value="div"]]
        |                  /argument[2]""".stripMargin

    val nodes = evaluatePathExpression(pmv, expr)

    nodes.head.update("[ Html.text \"Hello World\" ]")

    val newContent = pmv.findFile("src/BeginnerProgram.elm").content
    assert(newContent.contains("Hello World"))

    println("New content: ------\n" + newContent + "\n--------")

  }

}
