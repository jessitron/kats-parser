package com.jessitron.kats

import scala.util.parsing.combinator.RegexParsers

object ElmParser extends RegexParsers {

  def uppercaseIdentifier(name: String) : Parser[PositionedSyntaxNode] = positionedNode("[A-Z][A-Za-z0-9_]*".r ^^ SyntaxNode.leaf(name))

  def lowercaseIdentifier(name: String) : Parser[PositionedSyntaxNode] = positionedNode("[a-z][A-Za-z0-9_]*".r ^^ SyntaxNode.leaf(name))

  def exposure = lowercaseIdentifier("exposedFunction") | uppercaseIdentifier("exposedType")

  def exposings: Parser[Seq[SyntaxNode]] = "exposing" ~ "(" ~> rep1sep(exposure, ",") <~ ")"


  def moduleDeclaration: Parser[PositionedSyntaxNode] = positionedNode("module" ~> uppercaseIdentifier("moduleName") ~ exposings ^^
  { case name ~ exposings => SyntaxNode.parent("moduleDeclaration", Seq(name) ++  exposings)})

  def importStatement: Parser[PositionedSyntaxNode] = positionedNode("import" ~> qualifiedUppercaseIdentifier("importName") ~ opt(exposings) ^^
    { case name ~ exposings => SyntaxNode.parent("importStatement", Seq(name) ++ exposings.toSeq.flatten)})


  import ElmFunction.functionDeclaration
  def elmModule: Parser[PositionedSyntaxNode] = positionedNode(moduleDeclaration ~ rep(importStatement) ~ rep(functionDeclaration) ^^ {
    case name ~ imports ~ fns => SyntaxNode.parent("elmModule", Seq(name) ++ imports ++ fns)
  })


  def qualifiedLowercaseIdentifier(name: String): Parser[PositionedSyntaxNode] = positionedNode(opt(rep1sep(uppercaseIdentifier("component"), ".") <~ ".") ~ lowercaseIdentifier(name) ^^ {
    case None ~ p => SyntaxNode.parent(name, Seq(p))
    case Some(packages) ~ p => SyntaxNode.parent(name, packages :+ p)
  })

  def qualifiedUppercaseIdentifier(name: String): Parser[PositionedSyntaxNode] = positionedNode(rep1sep(uppercaseIdentifier("component"), ".") ^^ {
    SyntaxNode.parent(name, _)
  })

  object ElmExpression {

    def expression(name: String): Parser[PositionedSyntaxNode] = positionedNode((functionApplication | listLiteral | stringLiteral) ^^ {
      case expr => SyntaxNode.parent(name, Seq(expr))
    })

    def functionApplication: Parser[PositionedSyntaxNode] = positionedNode(qualifiedLowercaseIdentifier("calledFunction") ~ rep(expression("argument")) ^^ {
      case fn ~ args => SyntaxNode.parent("functionApplication", fn +: args)
    })

    def listLiteral: Parser[PositionedSyntaxNode] = positionedNode("[" ~> repsep(expression("listItem"), ",") <~ "]" ^^ {
      items => SyntaxNode.parent("listLiteral", items)
    })

    def stringLiteral: Parser[PositionedSyntaxNode] = positionedNode(""""[^"]*"""".r ^^ SyntaxNode.leaf("stringLiteral"))

  }

  import ElmExpression.expression

  object ElmFunction {

    def functionDeclaration: Parser[PositionedSyntaxNode] =
      positionedNode(opt(functionTypeDeclaration) ~ lowercaseIdentifier("functionName") ~ "=" ~ expression("body") ^^ {
        case typeOption ~ name ~ "=" ~ body => SyntaxNode.parent("functionDeclaration", typeOption.toSeq ++ Seq(name, body))
      })

    private    def functionTypeDeclaration: Parser[PositionedSyntaxNode] = positionedNode(lowercaseIdentifier("functionName") ~ ":" ~ elmType("declaredType") ^^ {
      case name ~ ":" ~ typ => SyntaxNode.parent("typeDeclaration", Seq(name, typ))
    })

    private    def elmType(name: String): Parser[PositionedSyntaxNode] = positionedNode(uppercaseIdentifier("typeName") ~ rep(elmType("typeParameter")) ^^ {
      case typeName ~ parameters => SyntaxNode.parent(name, typeName +: parameters)
    })

  }



  def parse(content: String): SyntaxNode = {
    parseAll(elmModule, content) match {
      case Success(result, next) => result
      case _: NoSuccess => ???
    }
  }

  def positionedNode(inner: Parser[SyntaxNode] ) = new Parser[PositionedSyntaxNode] {
    override def apply(in: Input): ParseResult[PositionedSyntaxNode] = {
      val start = handleWhiteSpace(in.source, in.offset)
      inner.apply(in) match {
        case Error(msg, next) => Error(msg, next)
        case Failure(msg, next) => Failure(msg, next)
        case Success(result, next) =>
          val end = next.offset
          Success(PositionedSyntaxNode(result.name, result.childNodes, result.valueOption,
            start, end), next)
      }
    }
  }


}

case class SyntaxNode(name: String,
                      childNodes: Seq[PositionedSyntaxNode],
                      valueOption: Option[String])


object SyntaxNode {

  def leaf(name: String)(value: String): SyntaxNode =
    SyntaxNode(name, Seq(), Some(value))

  def parent(name: String, children: Seq[SyntaxNode]): SyntaxNode =
    SyntaxNode(name, children, None)
}

case class PositionedSyntaxNode(name: String,
                                childNodes: Seq[PositionedSyntaxNode],
                                valueOption: Option[String],
                                start: Int,
                                end: Int)