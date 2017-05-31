package com.jessitron.kats

import com.atomist.rug.kind.grammar.ParsedNode
import com.jessitron.kats.ElmParser.ElmTypes.elmType
import com.jessitron.kats.ElmParser.positionedNode

import scala.util.parsing.combinator.RegexParsers

object ElmParser extends RegexParsers {

  def uppercaseIdentifier(name: String): Parser[PositionedSyntaxNode] = positionedNode("[A-Z][A-Za-z0-9_]*".r ^^ SyntaxNode.leaf(name))

  def lowercaseIdentifier(name: String): Parser[PositionedSyntaxNode] = positionedNode("[a-z][A-Za-z0-9_]*".r ^^ SyntaxNode.leaf(name))

  def exposure = lowercaseIdentifier("exposedFunction") | uppercaseIdentifier("exposedType")

  def exposings: Parser[Seq[PositionedSyntaxNode]] = "exposing" ~ "(" ~> rep1sep(exposure, ",") <~ ")"


  def moduleDeclaration: Parser[PositionedSyntaxNode] =
    positionedNode("module" ~> uppercaseIdentifier("moduleName") ~ exposings ^^ { case name ~ exposings => SyntaxNode.parent("moduleDeclaration", Seq(name) ++ exposings) })

  def importStatement: Parser[PositionedSyntaxNode] =
    positionedNode("import" ~> qualifiedUppercaseIdentifier("importName") ~ opt(exposings) ^^ { case name ~ exposings => SyntaxNode.parent("importStatement", Seq(name) ++ exposings.toSeq.flatten) })


  import ElmFunction.functionDeclaration

  def elmModule: Parser[PositionedSyntaxNode] =
    positionedNode(moduleDeclaration ~ rep(importStatement) ~ rep(functionDeclaration) ~ rep(section) ^^ {
      case name ~ imports ~ fns ~ sections => SyntaxNode.parent("elmModule", Seq(name) ++ imports ++ fns ++ sections)
    })

  def freeText = "[^\n]*\n".r ^^ { line => line.substring(0, line.length - 1) }

  def sectionHeader =
    positionedNode(("--" ~> freeText) ^^ SyntaxNode.leaf("sectionHeader"))

  import ElmTypes.typeAliasDeclaration

  def section =
    positionedNode(sectionHeader ~ rep(functionDeclaration | typeAliasDeclaration) ^^ { case (header ~ fns) => SyntaxNode.parent("section", header +: fns) })


  def qualifiedLowercaseIdentifier(name: String): Parser[PositionedSyntaxNode] = positionedNode(opt(rep1sep(uppercaseIdentifier("component"), ".") <~ ".") ~ lowercaseIdentifier(name) ^^ {
    case None ~ p => SyntaxNode.parent(name, Seq(p))
    case Some(packages) ~ p => SyntaxNode.parent(name, packages :+ p)
  })

  def qualifiedUppercaseIdentifier(name: String): Parser[PositionedSyntaxNode] = positionedNode(rep1sep(uppercaseIdentifier("component"), ".") ^^ {
    SyntaxNode.parent(name, _)
  })

  object ElmExpression {

    def expression(name: String): Parser[PositionedSyntaxNode] =
      positionedNode((functionApplication | listLiteral | stringLiteral | recordLiteral | tupleLiteral | hint("an Elm Expression")) ^^ {
        case expr => SyntaxNode.parent(name, Seq(expr))
      })

    def functionApplication: Parser[PositionedSyntaxNode] = positionedNode(qualifiedLowercaseIdentifier("calledFunction") ~ rep(expression("argument")) ^^ {
      case fn ~ args => SyntaxNode.parent("functionApplication", fn +: args)
    })

    def listLiteral: Parser[PositionedSyntaxNode] = positionedNode("[" ~> repsep(expression("listItem"), ",") <~ "]" ^^ {
      items => SyntaxNode.parent("listLiteral", items)
    })

    def stringLiteral: Parser[PositionedSyntaxNode] = positionedNode(""""[^"]*"""".r ^^ SyntaxNode.leaf("stringLiteral"))

    private def recordLiteralField: Parser[PositionedSyntaxNode] = positionedNode(lowercaseIdentifier("fieldName") ~ "=" ~ expression("fieldValue") ^^ {
      case name ~ _ ~ typ => SyntaxNode.parent("recordLiteralField", Seq(name, typ))
    })

    private def recordLiteral: Parser[PositionedSyntaxNode] =
      positionedNode("{" ~> rep(recordLiteralField) <~ "}" ^^ { fields => SyntaxNode.parent("recordLiteral", fields) })

    private def tupleLiteral: Parser[PositionedSyntaxNode] = positionedNode("(" ~> rep1sep(expression("tuplePart"), ",") <~ ")" ^^ { parts =>
      SyntaxNode.parent("tupleLiteral", parts)
    })
  }

  import ElmExpression.expression

  object ElmFunction {

    import ElmTypes._

    def functionDeclaration: Parser[PositionedSyntaxNode] =
      positionedNode(opt(functionTypeDeclaration) ~ lowercaseIdentifier("functionName") ~ "=" ~ expression("body") ^^ {
        case typeOption ~ name ~ "=" ~ body => SyntaxNode.parent("functionDeclaration", typeOption.toSeq ++ Seq(name, body))
      })

    private def functionTypeDeclaration: Parser[PositionedSyntaxNode] = positionedNode(lowercaseIdentifier("functionName") ~ ":" ~ elmType("declaredType") ^^ {
      case name ~ ":" ~ typ => SyntaxNode.parent("typeDeclaration", Seq(name, typ))
    })

  }

  object ElmTypes {

    def elmType(name: String): Parser[PositionedSyntaxNode] = positionedNode((typeReference | recordType | tupleType | hint("an Elm Type")) ^^ {
      typ => SyntaxNode.parent(name, Seq(typ))
    })

    private def typeReference: Parser[PositionedSyntaxNode] = positionedNode(uppercaseIdentifier("typeName") ~ rep(elmType("typeParameter")) ^^ {
      case typeName ~ parameters => SyntaxNode.parent("typeReference", typeName +: parameters)
    })

    private def tupleType: Parser[PositionedSyntaxNode] = positionedNode("(" ~> rep1sep(elmType("tupleTypePart"), ",") <~ ")" ^^ { parts =>
      SyntaxNode.parent("tupleType", parts)
    })

    private def recordTypeFieldDeclaration: Parser[PositionedSyntaxNode] = positionedNode(lowercaseIdentifier("fieldNameDeclaration") ~ ":" ~ elmType("fieldTypeDeclaration") ^^ {
      case name ~ _ ~ typ => SyntaxNode.parent("recordTypeField", Seq(name, typ))
    })

    private def recordType: Parser[PositionedSyntaxNode] =
      positionedNode("{" ~> rep(recordTypeFieldDeclaration) <~ "}" ^^ { fields => SyntaxNode.parent("recordType", fields) })

    def typeAliasDeclaration: Parser[PositionedSyntaxNode] =
      positionedNode(
        "type alias" ~> uppercaseIdentifier("typeName") ~ "=" ~ elmType("definition") ^^ {
          case name ~ _ ~ definition => SyntaxNode.parent("typeAlias", Seq(name, definition))
        }
      )
  }

  def hint(clue: String): Parser[PositionedSyntaxNode] = clue ^^ {
    _ => throw new RuntimeException(s"I want an example of ${clue}, not literally '${clue}'")
  }

  def parse(content: String): PositionedSyntaxNode = {
    parseAll(elmModule, content) match {
      case Success(result, next) => result
      case x: NoSuccess => throw new RuntimeException(x.toString)
    }
  }

  def positionedNode(inner: Parser[SyntaxNode]) = new Parser[PositionedSyntaxNode] {
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

  def parent(name: String, children: Seq[PositionedSyntaxNode]): SyntaxNode =
    SyntaxNode(name, children, None)
}

case class PositionedSyntaxNode(override val nodeName: String,
                                override val parsedNodes: Seq[PositionedSyntaxNode],
                                valueOption: Option[String],
                                override val startOffset: Int,
                                override val endOffset: Int) extends ParsedNode