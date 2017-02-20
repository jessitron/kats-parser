package com.jessitron.kats

import com.atomist.rug.kind.RugLanguageExtensionTest
import org.scalatest.FlatSpec

class ElmTypeTest extends FlatSpec with RugLanguageExtensionTest {

  it should "change my source code" in {
    val sourceProjectLocation = "/Users/jessitron/code/elm/Kats"

    val pmv = projectFromDirectory(sourceProjectLocation)

    val expr = """/src//Elm()//functionApplication[/calledFunction[@value="text"]]/argument/stringLiteral"""

    val nodes = evaluatePathExpression(pmv, expr)

    nodes.head.update("\"Hello Mike\"")

    val newContent = pmv.findFile("src/Main.elm").content
    assert(newContent.contains("Mike"))

    println("New content: ------\n" + newContent + "\n--------")

  }

}
