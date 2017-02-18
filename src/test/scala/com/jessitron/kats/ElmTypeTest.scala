package com.jessitron.kats

import com.atomist.rug.kind.RugLanguageExtensionTest
import org.scalatest.FlatSpec

class ElmTypeTest extends FlatSpec with RugLanguageExtensionTest {


  it should "change my source code" in {
    val sourceProjectLocation = ???

    val pmv = projectFromDirectory(sourceProjectLocation)

    val expr = """//Elm()"""

    val nodes = evaluatePathExpression(pmv, expr)

    nodes.head.update("I am the new value!")

    val newContent = pmv.findFile(???).content
    assert(???)

    println("New content: ------\n" + newContent + "\n--------")

  }

}
