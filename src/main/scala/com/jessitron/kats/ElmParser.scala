package com.jessitron.kats

import com.atomist.rug.kind.grammar.ParsedNode

import scala.util.parsing.combinator.RegexParsers

object ElmParser extends RegexParsers {

  def uppercaseIdentifier(name: String): Parser[PositionedSyntaxNode] = positionedNode("[A-Z][A-Za-z0-9_]*".r ^^ SyntaxNode.leaf(name))

  def lowercaseIdentifier(name: String): Parser[PositionedSyntaxNode] = positionedNode(identifier ^^ SyntaxNode.leaf(name))

  protected def identifier = new Parser[String] {
    val underlying = "[a-z][A-Za-z0-9_]*".r
    val reservedWords = Seq("type", "case", "of")

    def apply(in: Input): ParseResult[String] = {
      val pr = underlying.apply(in)
      pr match {
        case succ: Success[String@unchecked] =>
          if (reservedWords.contains(succ.get))
            Failure(s"Cannot use reserved word '${succ.get}' as identifier", succ.next)
          else
            Success[String](succ.get, succ.next)
        case f: Failure => f
        case _ => ???
      }
    }
  }

  def exposure = lowercaseIdentifier("exposedFunction") | uppercaseIdentifier("exposedType") | exposeAll

  def exposeAll = positionedNode(".." ^^ SyntaxNode.leaf("exposeAll"))

  def exposings: Parser[Seq[PositionedSyntaxNode]] = "exposing" ~ "(" ~> rep1sep(exposure, ",") <~ ")"

  def docString: Parser[PositionedSyntaxNode] = positionedNode(("{-|" ~> "[\\s\\S]*?\\-\\}".r) ^^ SyntaxNode.leaf("docstring"))

  def moduleDeclaration: Parser[PositionedSyntaxNode] =
    positionedNode("module" ~> uppercaseIdentifier("moduleName") ~ exposings ~ opt(docString) ^^ {
      case name ~ exposings ~ doc => SyntaxNode.parent("moduleDeclaration", Seq(name) ++ exposings ++ doc.toSeq) })

  def importStatement: Parser[PositionedSyntaxNode] =
    positionedNode("import" ~> qualifiedUppercaseIdentifier("importName") ~ opt(exposings) ^^ { case name ~ exposings => SyntaxNode.parent("importStatement", Seq(name) ++ exposings.toSeq.flatten) })


  import ElmFunction.functionDeclaration

  def elmModule: Parser[PositionedSyntaxNode] =
    positionedNode(moduleDeclaration ~ rep(importStatement) ~ rep(topLevel) ~ rep(section) ^^ {
      case name ~ imports ~ fns ~ sections => SyntaxNode.parent("elmModule", Seq(name) ++ imports ++ fns ++ sections)
    })

  def freeText = "[^\n]*\n".r ^^ { line => line.substring(0, line.length - 1) }

  def sectionHeader =
    positionedNode(("--" ~> freeText) ^^ SyntaxNode.leaf("sectionHeader"))

  import ElmTypes.typeAliasDeclaration
  import ElmTypes.unionTypeDeclaration

  def section =
    positionedNode(sectionHeader ~ rep(topLevel) ^^ { case (header ~ fns) => SyntaxNode.parent("section", header +: fns) })


  def topLevel = functionDeclaration | typeAliasDeclaration | unionTypeDeclaration

  def qualifiedLowercaseIdentifier(name: String): Parser[PositionedSyntaxNode] = positionedNode(opt(rep1sep(uppercaseIdentifier("component"), ".") <~ ".") ~ lowercaseIdentifier(name) ^^ {
    case None ~ p => SyntaxNode.parent(name, Seq(p))
    case Some(packages) ~ p => SyntaxNode.parent(name, packages :+ p)
  })

  def qualifiedUppercaseIdentifier(name: String): Parser[PositionedSyntaxNode] = positionedNode(rep1sep(uppercaseIdentifier("component"), ".") ^^ {
    SyntaxNode.parent(name, _)
  })

  object ElmExpression {

    def expression(name: String): Parser[PositionedSyntaxNode] =
      positionedNode((functionApplication | listLiteral | stringLiteral | recordLiteral | tupleLiteral | switch | hint("an Elm Expression")) ^^ {
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
      positionedNode("{" ~> repsep(recordLiteralField,",") <~ "}" ^^ { fields => SyntaxNode.parent("recordLiteral", fields) })

    private def tupleLiteral: Parser[PositionedSyntaxNode] =
      positionedNode("(" ~> rep1sep(expression("tuplePart"), ",") <~ ")" ^^ { parts =>
        SyntaxNode.parent("tupleLiteral", parts)
      })

    private def switch: Parser[PositionedSyntaxNode] =
      positionedNode("case" ~> expression("pivot") ~ "of" ~ rep(switchClause) ^^ {
        case pivot ~ _ ~ clauses => SyntaxNode.parent("caseExpression", pivot +: clauses)
      })

    import ElmDecomposition.matchable

    private def switchClause: Parser[PositionedSyntaxNode] =
      positionedNode(matchable ~ "->" ~ expression("result") ^^ {
        case pattern ~ _ ~ result => SyntaxNode.parent("clause", Seq(pattern, result))
      })
  }

  import ElmExpression.expression

  object ElmFunction {

    import ElmTypes._
    import ElmDecomposition._

    def functionDeclaration: Parser[PositionedSyntaxNode] =
      positionedNode(opt(docString) ~ opt(functionTypeDeclaration) ~ lowercaseIdentifier("functionName") ~ rep(matchable) ~ "=" ~ expression("body") ^^ {
        case docStringOption ~ typeOption ~ name ~ params ~ "=" ~ body =>
          SyntaxNode.parent("functionDeclaration",
            docStringOption.toSeq ++ typeOption.toSeq ++ (name +: params :+ body))
      })

    private def functionTypeDeclaration: Parser[PositionedSyntaxNode] = positionedNode(lowercaseIdentifier("functionName") ~ ":" ~ elmType("declaredType") ^^ {
      case name ~ ":" ~ typ => SyntaxNode.parent("typeDeclaration", Seq(name, typ))
    })

  }

  object ElmDecomposition {
    def matchable: Parser[PositionedSyntaxNode] = constructor | matchableExceptConstructor

    def matchableExceptConstructor = lowercaseIdentifier("identifier") | hint("a pattern")

    private def constructor = positionedNode(uppercaseIdentifier("constructor") ~ rep(matchableExceptConstructor) ^^ {
      case name ~ patterns => SyntaxNode.parent("constructorPattern", name +: patterns)
    } )
  }

  object ElmTypes {

    def elmType(name: String): Parser[PositionedSyntaxNode] = positionedNode((functionType | elmTypeExceptFunction) ^^ {
      typ => SyntaxNode.parent(name, Seq(typ))
    })

    def elmTypeExceptFunction: Parser[PositionedSyntaxNode] =
      typeReference | variableTypeReference| recordType | tupleType | parensAroundType | hint("an Elm Type")

    private def parensAroundType: Parser[PositionedSyntaxNode] = "(" ~> elmType("insideParens") <~ ")"

    private def functionType: Parser[PositionedSyntaxNode] =
      positionedNode(rep1sep(elmTypeExceptFunction, "->") ^^ {
        case one if one.length == 1 => SyntaxNode.unposition(one.head)
        case more => SyntaxNode.parent("functionType", more)
      })

    private def typeReference: Parser[PositionedSyntaxNode] =
      positionedNode(uppercaseIdentifier("typeName") ~ rep(elmType("typeParameter")) ^^ {
      case typeName ~ parameters => SyntaxNode.parent("typeReference", typeName +: parameters)
    })

    private def variableTypeReference = lowercaseIdentifier("typeVariable")

    private def tupleType: Parser[PositionedSyntaxNode] =
      positionedNode("(" ~> rep1sep(elmType("tupleTypePart"), ",") <~ ")" ^^ { parts =>
      SyntaxNode.parent("tupleType", parts)
    })

    private def recordTypeFieldDeclaration: Parser[PositionedSyntaxNode] =
      positionedNode(lowercaseIdentifier("fieldNameDeclaration") ~ ":" ~ elmType("fieldTypeDeclaration") ^^ {
      case name ~ _ ~ typ => SyntaxNode.parent("recordTypeField", Seq(name, typ))
    })

    private def recordType: Parser[PositionedSyntaxNode] =
      positionedNode("{" ~> repsep(recordTypeFieldDeclaration,",") <~ "}" ^^ {
        fields => SyntaxNode.parent("recordType", fields) })

    def typeAliasDeclaration: Parser[PositionedSyntaxNode] =
      positionedNode(
        opt(docString) ~ "type alias" ~ uppercaseIdentifier("typeName") ~ "=" ~ elmType("definition") ^^ {
          case docString ~ _ ~ name ~ _ ~ definition =>
            SyntaxNode.parent("typeAlias", docString.toSeq ++ Seq(name, definition))
        }
      )

    def unionTypeDeclaration: Parser[PositionedSyntaxNode] =
      positionedNode(opt(docString) ~ "type" ~ uppercaseIdentifier("typeName") ~ rep(lowercaseIdentifier("typeParameter")) ~ "=" ~ rep1sep(elmType("constructor"), "|") ^^ {
        case docString ~ _ ~ name ~ params ~  _ ~ constructors =>
          SyntaxNode.parent("unionTypeDeclaration",
            docString.toSeq ++ Seq(name) ++ params ++ constructors)
      })
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

  def unposition(positionedSyntaxNode: PositionedSyntaxNode): SyntaxNode = {
    SyntaxNode(positionedSyntaxNode.nodeName, Seq(), positionedSyntaxNode.valueOption)
  }
}

case class PositionedSyntaxNode(override val nodeName: String,
                                override val parsedNodes: Seq[PositionedSyntaxNode],
                                valueOption: Option[String],
                                override val startOffset: Int,
                                override val endOffset: Int) extends ParsedNode