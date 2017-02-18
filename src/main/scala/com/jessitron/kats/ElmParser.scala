package com.jessitron.kats

import scala.util.parsing.combinator.RegexParsers

object ElmParser extends RegexParsers {

  def uppercaseIdentifier(name: String) : Parser[SyntaxNode] = "[A-Z][A-Za-z0-9_]*".r ^^ SyntaxNode.leaf(name)

  def moduleDeclaration: Parser[SyntaxNode] = "module" ~> uppercaseIdentifier("moduleName")

  def elmModule: Parser[SyntaxNode] = moduleDeclaration ~ everything ^^ {
    case name ~ rest => SyntaxNode.parent("elmModule", Seq(name, rest))
  }

  def everything: Parser[SyntaxNode] = "(.|\n)+".r ^^ SyntaxNode.leaf("everything")

  def parse(content: String): SyntaxNode = {
    parseAll(elmModule, content) match {
      case Success(result, next) => result
      case _: NoSuccess => ???
    }
  }

}

case class SyntaxNode(name: String,
                      childNodes: Seq[SyntaxNode],
                      valueOption: Option[String])

object SyntaxNode {

  def leaf(name: String)(value: String): SyntaxNode =
    SyntaxNode(name, Seq(), Some(value))

  def parent(name: String, children: Seq[SyntaxNode]): SyntaxNode =
    SyntaxNode(name, children, None)
}
