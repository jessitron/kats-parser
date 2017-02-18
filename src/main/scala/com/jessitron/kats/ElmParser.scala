package com.jessitron.kats

import scala.util.parsing.combinator.RegexParsers

object ElmParser extends RegexParsers {

  def uppercaseIdentifier(name: String) : Parser[SyntaxNode] = "[A-Z][A-Za-z0-9_]*".r ^^ SyntaxNode.leaf(name)

  def lowercaseIdentifier(name: String) : Parser[SyntaxNode] = "[a-z][A-Za-z0-9_]*".r ^^ SyntaxNode.leaf(name)

  def exposure = lowercaseIdentifier("exposedFunction") | uppercaseIdentifier("exposedType")

  def exposings: Parser[Seq[SyntaxNode]] = "exposing" ~ "(" ~> rep1sep(exposure, ",") <~ ")"


  def moduleDeclaration: Parser[SyntaxNode] = "module" ~> uppercaseIdentifier("moduleName") ~ exposings ^^
  { case name ~ exposings => SyntaxNode.parent("moduleDeclaration", Seq(name) ++  exposings)}

  def importStatement: Parser[SyntaxNode] = "import" ~> qualifiedUppercaseIdentifier("importName") ~ opt(exposings) ^^
    { case name ~ exposings => SyntaxNode.parent("importStatement", Seq(name) ++ exposings.toSeq.flatten)}


  import ElmFunction.functionDeclaration
  def elmModule: Parser[SyntaxNode] = moduleDeclaration ~ rep(importStatement) ~ rep(functionDeclaration) ^^ {
    case name ~ imports ~ fns => SyntaxNode.parent("elmModule", Seq(name) ++ imports ++ fns)
  }


  def qualifiedLowercaseIdentifier(name: String): Parser[SyntaxNode] = opt(rep1sep(uppercaseIdentifier("component"), ".") <~ ".") ~ lowercaseIdentifier(name) ^^ {
    case None ~ p => SyntaxNode.parent(name, Seq(p))
    case Some(packages) ~ p => SyntaxNode.parent(name, packages :+ p)
  }

  def qualifiedUppercaseIdentifier(name: String): Parser[SyntaxNode] = rep1sep(uppercaseIdentifier("component"), ".") ^^ {
    SyntaxNode.parent(name, _)
  }

  object ElmExpression {

    def expression(name: String): Parser[SyntaxNode] = (functionApplication | listLiteral | stringLiteral) ^^ {
      case expr => SyntaxNode.parent(name, Seq(expr))
    }

    def functionApplication: Parser[SyntaxNode] = qualifiedLowercaseIdentifier("calledFunction") ~ rep(expression("argument")) ^^ {
      case fn ~ args => SyntaxNode.parent("functionApplication", fn +: args)
    }

    def listLiteral: Parser[SyntaxNode] = "[" ~> repsep(expression("listItem"), ",") <~ "]" ^^ {
      items => SyntaxNode.parent("listLiteral", items)
    }

    def stringLiteral: Parser[SyntaxNode] = """"[^"]*"""".r ^^ SyntaxNode.leaf("stringLiteral")

  }

  import ElmExpression.expression

  object ElmFunction {

    def functionDeclaration: Parser[SyntaxNode] =
      opt(functionTypeDeclaration) ~ lowercaseIdentifier("functionName") ~ "=" ~ expression("body") ^^ {
        case typeOption ~ name ~ "=" ~ body => SyntaxNode.parent("functionDeclaration", typeOption.toSeq ++ Seq(name, body))
      }

    private  def functionTypeDeclaration: Parser[SyntaxNode] = lowercaseIdentifier("functionName") ~ ":" ~ elmType("declaredType") ^^ {
      case name ~ ":" ~ typ => SyntaxNode.parent("typeDeclaration", Seq(name, typ))
    }

    private  def elmType(name: String): Parser[SyntaxNode] = uppercaseIdentifier("typeName") ~ rep(elmType("typeParameter")) ^^ {
      case typeName ~ parameters => SyntaxNode.parent(name, typeName +: parameters)
    }

  }



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
