package com.jessitron.kats

import com.atomist.rug.kind.grammar.ParsedNode

import scala.util.parsing.combinator.RegexParsers

object ElmProcessor {

  def addSpecials(originalContent: String): String =
    markMovesToTheLeft(markBeginningOfLine(originalContent))

  def removeSpecials(preprocessedContent: String): String =
    preprocessedContent.replaceAll("[☞❡]", "")

  private def markBeginningOfLine(originalContent: String): String = {
    originalContent.replaceAll("(?m)^(\\S)", "\\☞$1")
  }

  private def markMovesToTheLeft(originalContent: String): String = {
    val lines = originalContent.lines.foldLeft[(Int, Seq[String])]((0, Seq())) {
      case ((prevIndent, lines), current) =>
        if (current.trim().isEmpty || current.trim().startsWith("--"))
          (prevIndent, lines :+ current)
        else {
          val currentIndent = current.indexOf(current.trim())
          val thisLine = if (0 < currentIndent && currentIndent < prevIndent) "❡" + current else current
          (currentIndent, lines :+ thisLine)
        }
    }
    lines._2.mkString("\n")
  }
}

object ElmParser extends RegexParsers {

  val VERSION = "0.2.8"

  val infixFunctionRegex: Parser[String] = "[\\+\\*<>&=/|^%:!]+".r | "-"

  val weirdFunctionName: Parser[String] = "(,)" // treating this with other infixes makes record parsing harder

  def uppercaseIdentifier(name: String): Parser[PositionedSyntaxNode] = positionedNode("[A-Z][A-Za-z0-9_]*".r ^^ SyntaxNode.leaf(name))

  def lowercaseIdentifier(name: String): Parser[PositionedSyntaxNode] = positionedNode(identifier ^^ SyntaxNode.leaf(name))

  protected def identifier = new Parser[String] {
    val underlying = "[a-z][A-Za-z0-9_]*".r
    val reservedWords = Seq("if", "then", "else", "type", "case", "of", "let", "in", "infixr", "infixl", "infix", "as")

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


  def functionName: Parser[PositionedSyntaxNode] = {
    def infixFunctionTechnicalName: Parser[String] = "(" ~ infixFunctionRegex ~ ")" ^^ { case a ~ b ~ c => a + b + c }

    positionedNode((identifier | infixFunctionTechnicalName | weirdFunctionName) ^^ SyntaxNode.leaf("functionName"))
  }

  def docString: Parser[PositionedSyntaxNode] = positionedNode(("☞{-|" ~> "[\\s\\S]*?\\-\\}".r) ^^ SyntaxNode.leaf("docstring"))

  object Module {

    private def exposedType =
      positionedNode(uppercaseIdentifier("type") ~ opt("(" ~> rep1sep(uppercaseIdentifier("constructor") | exposeAll, commaSeparator) <~ opt("moveLeft") ~ ")") ^^ {
        case typ ~ None => SyntaxNode.parent("exposedType", Seq(typ))
        case typ ~ Some(constructors) => SyntaxNode.parent("exposedType", typ +: constructors)
      })

    private def exposure = functionName | exposedType | exposeAll

    private def exposeAll = positionedNode(".." ^^ SyntaxNode.leaf("exposeAll"))

    private def exposings: Parser[PositionedSyntaxNode] = positionedNode("exposing" ~ "(" ~> rep1sep(exposure, commaSeparator) <~ ")" ^^ {
      exp => SyntaxNode.parent("exposing", exp)
    })

    private def where = wrap("where", "where" ~> ElmExpression.recordLiteral)

    def moduleDeclaration: Parser[PositionedSyntaxNode] =
      positionedNode("☞" ~ opt("effect") ~ "module" ~> qualifiedUppercaseIdentifier("moduleName") ~ opt(where) ~ exposings ~ opt(docString) ^^ {
        case name ~ where ~ exposings ~ doc => SyntaxNode.parent("moduleDeclaration", Seq(name) ++ where.toSeq ++ Seq(exposings) ++ doc.toSeq)
      })

    def importStatement: Parser[PositionedSyntaxNode] =
      positionedNode("☞import" ~> qualifiedUppercaseIdentifier("importName") ~ opt(exposings) ~ opt("as" ~> uppercaseIdentifier("alias")) ^^ {
        case name ~ exposings ~ alias => SyntaxNode.parent("import", Seq(name) ++ exposings.toSeq ++ alias.toSeq)
      })

  }

  def printAttempt[T](message: String): Parser[T] = new Parser[T] {
    override def apply(in: ElmParser.Input): ElmParser.ParseResult[T] = {
      println(s"${message} attempting to match at [${in.pos.line}, ${in.pos.column}]")
      Failure("nah", in)
    }
  }

  import TopLevel._
  import Module._

  def elmModule: Parser[PositionedSyntaxNode] =
    positionedNode(moduleDeclaration ~ rep(importStatement) ~ moduleBody ^^ {
      case name ~ imports ~ body => SyntaxNode.parent("elmModule", Seq(name) ++ imports ++ Seq(body))
    })

  private def moduleBody: Parser[PositionedSyntaxNode] =
    positionedNode(rep(topLevel) ~ rep(section) ^^ {
      case preSectionDeclarations ~ sections => SyntaxNode.parent("moduleBody", preSectionDeclarations ++ sections)
    })

  private def sectionHeader =
    "☞--+".r ~> positionedNode(("[A-Z\\s]+?\n".r) ^^ SyntaxNode.leaf("sectionHeader")).map(_.chomp)

  private def sectionContent: Parser[PositionedSyntaxNode] =
    positionedNode(rep(topLevel) ^^ { content =>
      SyntaxNode.parent("sectionContent", content)
    })


  def section =
    positionedNode(sectionHeader ~ sectionContent ^^ {
      case (header ~ fns) => SyntaxNode.parent("section", Seq(header, fns))
    })


  def qualifiedFunctionName: Parser[PositionedSyntaxNode] = positionedNode(opt(rep1sep(uppercaseIdentifier("component"), ".") <~ ".") ~ functionName ^^ {
    case None ~ p => SyntaxNode.unposition(p)
    case Some(packages) ~ p =>
      SyntaxNode.parent("qualifiedFunctionName", packages :+ p)
  })

  def qualifiedUppercaseIdentifier(name: String): Parser[PositionedSyntaxNode] =
    positionedNode(rep1sep(uppercaseIdentifier("component"), ".") ^^ {
      SyntaxNode.parent(name, _)
    })

  def comment: Parser[PositionedSyntaxNode] = {
    def restOfLine = "[^\n]*\n".r ^^ { line => line.substring(0, line.length - 1) }

    // TODO: these can be nested
    def delimitedComment = "\\{-[\\s\\S]*?-\\}".r

    def restOfLineComment = "--" ~> restOfLine.filter("^[A-Z\\s]+$".matches(_))

    positionedNode((delimitedComment | restOfLineComment) ^^ SyntaxNode.leaf("comment"))
  }

  def moveLeft: Parser[String] = "❡"

  def commaSeparator: Parser[String] = ","

  object ElmExpression {

    def expression(name: String): Parser[PositionedSyntaxNode] =
    // printAttempt(s"Looking for expression called ${name}: ") |
      positionedNode(opt(comment) ~> (
        infixFunctionApplication |
          fieldAccess |
          functionApplication | // this must be before constructorApplication|
          elmExpressionWithClearPrecedence
        ) <~ opt(comment) ^^ {
        case expr => SyntaxNode.parent(name, Seq(expr))
      })

    private def elmExpressionWithClearPrecedence: Parser[PositionedSyntaxNode] =
    // printAttempt("clear precedence:") |
      qualifiedFunctionName |
        constructorApplication |
        listLiteral |
        SimpleLiteral.literal |
        tupleLiteral |
        anonymousFunction |
        recordLiteral |
        ifExpression |
        switch |
        letExpression |
        expressionInParens |
        hint("an Elm Expression")

    private def elmExpressionThatIsClearEnoughToBeAnArgument =
      fieldAccess |
        qualifiedFunctionName |
        listLiteral |
        SimpleLiteral.literal |
        tupleLiteral |
        anonymousFunction |
        recordLiteral |
        constructorApplication |
        expressionInParens |
        hint("a function argument")

    private def elmExpressionThatMightResultInARecord: Parser[PositionedSyntaxNode] =
    // printAttempt("might be a record:") |
      qualifiedFunctionName |
        functionApplication |
        recordLiteral |
        ifExpression |
        switch |
        letExpression |
        expressionInParens

    private def fieldAccess =
    // printAttempt("field access:") |
      positionedNode(wrap("record", elmExpressionThatMightResultInARecord) ~ "." ~ lowercaseIdentifier("fieldName") ^^ {
        case record ~ _ ~ field => SyntaxNode.parent("recordFieldAccess", Seq(record, field))
      }, Some("fieldAccess"))

    private def expressionInParens = "(" ~> expression("insideParens") <~ opt(moveLeft) ~ ")"

    private def functionApplication: Parser[PositionedSyntaxNode] =
      positionedNode(
        wrap("function", elmExpressionWithClearPrecedence) ~
          rep1(wrap("argument", elmExpressionThatIsClearEnoughToBeAnArgument)) ^^ {
          case fn ~ args => SyntaxNode.parent("functionApplication", fn +: args)
        })

    private def constructorApplication = positionedNode((qualifiedUppercaseIdentifier("calledConstructor") ~ rep(expression("argument"))) ^^ {
      case fn ~ args => SyntaxNode.parent("constructorApplication", fn +: args)
    })

    private def infixFunction = positionedNode(infixFunctionRegex ^^ SyntaxNode.leaf("infixFunction"))

    private def infixFunctionApplication =
      positionedNode(wrap("argument", elmExpressionWithClearPrecedence) ~ infixFunction ~ expression("argument") ^^ {
        case left ~ fn ~ right => SyntaxNode.parent("functionApplication", Seq(left, fn, right))
      })

    import ElmDecomposition.matchable

    private def anonymousFunction =
      positionedNode("\\" ~> rep1(matchable) ~ "->" ~ expression("body") ^^ {
        case params ~ _ ~ body => SyntaxNode.parent("anonymousFunction", params :+ body);
      })

    private def listLiteral: Parser[PositionedSyntaxNode] =
      positionedNode("[" ~> repsep(expression("listItem"), opt(moveLeft) ~ commaSeparator) <~ opt(moveLeft) ~ "]" ^^ {
        items => SyntaxNode.parent("listLiteral", items)
      })

    object SimpleLiteral {

      def literal: Parser[PositionedSyntaxNode] = intLiteral | charLiteral | stringLiteral | floatLiteral | unit

      private def intLiteral: Parser[PositionedSyntaxNode] =
        positionedNode("[0-9]+".r ^^ SyntaxNode.leaf("intLiteral"))

      private def floatLiteral: Parser[PositionedSyntaxNode] =
        positionedNode("[0-9]+\\.[0-9]+".r ^^ SyntaxNode.leaf("intLiteral"))

      private def charLiteral: Parser[PositionedSyntaxNode] =
        positionedNode("'.'".r ^^ SyntaxNode.leaf("charLiteral"))

      private def stringLiteral: Parser[PositionedSyntaxNode] =
        positionedNode(""""[^"]*"""".r ^^ SyntaxNode.leaf("stringLiteral"))


    }

    private def unit = positionedNode("()" ^^ SyntaxNode.leaf("unit"))


    private def recordLiteralField: Parser[PositionedSyntaxNode] =
      positionedNode(lowercaseIdentifier("fieldName") ~ "=" ~ expression("fieldValue") ^^ {
        case name ~ _ ~ typ => SyntaxNode.parent("recordLiteralField", Seq(name, typ))
      })

    private def startingRecord = lowercaseIdentifier("startingRecord") <~ "|"

    def recordLiteral: Parser[PositionedSyntaxNode] =
      positionedNode("{" ~> opt(startingRecord) ~
        repsep(recordLiteralField, opt(moveLeft) ~ commaSeparator)
        <~ opt(moveLeft) ~ "}" ^^ {
        case Some(startingRecord) ~ fields =>
          SyntaxNode.parent("recordLiteral", startingRecord +: fields)
        case None ~ fields => SyntaxNode.parent("recordLiteral", fields)
      })

    private def tupleLiteral: Parser[PositionedSyntaxNode] =
      positionedNode("(" ~> rep1sep(expression("tuplePart"), opt(moveLeft) ~ commaSeparator) <~ opt("moveLeft") ~ ")" ^^ { parts =>
        SyntaxNode.parent("tupleLiteral", parts)
      })


    private def ifExpression =
      positionedNode("if" ~> expression("condition") ~ "then" ~ expression("thenBody") ~ opt(moveLeft) ~ "else" ~ expression("elseBody") ^^ {
        case condition ~ _ ~ ifBody ~ _ ~ _ ~ elseBody => SyntaxNode.parent("if", Seq(condition, ifBody, elseBody))
      })

    private def switch: Parser[PositionedSyntaxNode] =
      positionedNode("case" ~> expression("pivot") ~ "of" ~ rep(caseClause) ^^ {
        case pivot ~ _ ~ clauses => SyntaxNode.parent("caseExpression", pivot +: clauses)
      })

    import ElmDecomposition.matchable

    private def pattern: Parser[PositionedSyntaxNode] = wrap("pattern", matchable)

    private def caseClause: Parser[PositionedSyntaxNode] =
      positionedNode(opt(comment) ~ opt(moveLeft) ~> pattern ~ "->" ~ expression("result") ^^ {
        case pattern ~ _ ~ result =>
          //  println(s"matched switch clause: ${pattern.values} -> ${result.values}")
          SyntaxNode.parent("clause", Seq(pattern, result))
      })

    private def decomposition = positionedNode(matchable ~ "=" ~ expression("body") ^^ {
      case matchable ~ _ ~ body => SyntaxNode.parent("decomposingDeclaration", Seq(matchable, body))
    })

    private def letExpression = positionedNode(
      "let" ~> rep1(opt(moveLeft) ~> (functionDeclaration("") | decomposition)) ~ opt(moveLeft) ~ "in" ~ expression("body") ^^ {
        case declarations ~ _ ~ _ ~ body => SyntaxNode.parent("let", declarations :+ body)
      })
  }

  import ElmExpression.expression

  object TopLevel {

    import ElmTypes._
    import ElmDecomposition._

    def topLevel =
      functionDeclaration() |
        typeAliasDeclaration |
        unionTypeDeclaration |
        infixPrecedenceDeclaration |
        ("☞" ~> comment) |
        hint("top level declaration")

    def functionDeclaration(lineBegin: Parser[String] = "☞"): Parser[PositionedSyntaxNode] =
      positionedNode(opt(docString) ~ opt(functionTypeDeclaration(lineBegin)) ~ lineBegin ~ functionName ~ rep(matchable) ~ "=" ~ expression("body") ^^ {
        case docStringOption ~ typeOption ~ _ ~ name ~ params ~ "=" ~ body =>
          SyntaxNode.parent("functionDeclaration",
            docStringOption.toSeq ++ typeOption.toSeq ++ (name +: params :+ body))
      })

    private def functionTypeDeclaration(lineBegin: Parser[String]): Parser[PositionedSyntaxNode] =
      positionedNode(lineBegin ~> functionName ~ ":" ~ elmType("declaredType") ^^ {
        case name ~ ":" ~ typ => SyntaxNode.parent("typeDeclaration", Seq(name, typ))
      })


    private def typeAliasDeclaration: Parser[PositionedSyntaxNode] =
      positionedNode(
        opt(docString) ~ "☞type alias" ~ uppercaseIdentifier("typeName") ~ "=" ~ elmType("definition") ^^ {
          case docString ~ _ ~ name ~ _ ~ definition =>
            SyntaxNode.parent("typeAlias", docString.toSeq ++ Seq(name, definition))
        }
      )

    private def unionTypeDeclaration: Parser[PositionedSyntaxNode] =
      positionedNode(opt(docString) ~ "☞type" ~
        uppercaseIdentifier("typeName") ~ rep(lowercaseIdentifier("typeParameter")) ~ "=" ~
        rep1sep(elmType("constructor"), "|") ^^ {
        case docString ~ _ ~ name ~ params ~ _ ~ constructors =>
          SyntaxNode.parent("unionTypeDeclaration",
            docString.toSeq ++ Seq(name) ++ params ++ constructors)
      })

    private def infixPrecedenceDeclaration: Parser[PositionedSyntaxNode] = {

      def declaration = positionedNode(("infixl" | "infixr" | "infix") ^^ SyntaxNode.leaf("declaration"))

      def precedence = positionedNode("[0-9]".r ^^ SyntaxNode.leaf("precedence"))

      def infixFn = positionedNode(infixFunctionRegex ^^ SyntaxNode.leaf("infixFunction"))

      positionedNode(("☞" ~> declaration ~ precedence ~ infixFn) ^^ {
        case declaration ~ precedence ~ infixFn => SyntaxNode.parent("infixPrecedenceDeclaration",
          Seq(declaration, precedence, infixFn))
      })
    }


  }

  object ElmDecomposition {
    def matchable: Parser[PositionedSyntaxNode] =
      deconstructorPattern |
        consPattern |
        matchableWithClearPrecedence

    private def matchableWithClearPrecedence =
      lowercaseIdentifier("identifier") |
        ignored |
        aliasedMatchable |
        matcherInParens |
        tupleDecomposition |
        noArgConstructor |
        listDecomposition |
        recordDecomposition |
        ElmExpression.SimpleLiteral.literal |
        hint("a pattern")

    private def listDecomposition = positionedNode("[" ~> repsep(matchable, commaSeparator) <~ opt("moveLeft") ~ "]" ^^ {
      elems => SyntaxNode.parent("listPattern", elems)
    })

    private def recordDecomposition = positionedNode("{" ~> repsep(matchable, commaSeparator) <~ opt("moveLeft") ~ "}" ^^ {
      elems => SyntaxNode.parent("recordPattern", elems)
    })

    private def consPattern = positionedNode((matchableWithClearPrecedence ~ "::" ~ matchable) ^^ {
      case left ~ _ ~ right => SyntaxNode.parent("consPattern", Seq(left, right))
    })

    private def deconstructorPattern = positionedNode(qualifiedUppercaseIdentifier("constructorName") ~ rep1(matchableWithClearPrecedence) ^^ {
      case name ~ patterns => SyntaxNode.parent("deconstructor", name +: patterns)
    })

    private def matcherInParens = "(" ~> matchable <~ opt("moveLeft") ~ ")"

    private def aliasedMatchable = positionedNode(matcherInParens ~ "as" ~ lowercaseIdentifier("alias") ^^ {
      case matchable ~ _ ~ alias => SyntaxNode.parent("aliasedPattern", Seq(matchable, alias))
    })

    private def noArgConstructor = qualifiedUppercaseIdentifier("constructorName")

    private def ignored = positionedNode("_" ^^ SyntaxNode.leaf("ignored"))

    private def tupleDecomposition = positionedNode(("(" ~> rep1sep(matchable, commaSeparator) <~ opt("moveLeft") ~ ")") ^^ {
      inners => SyntaxNode.parent("tupleDecomposition", inners)
    })
  }

  object ElmTypes {

    def elmType(name: String): Parser[PositionedSyntaxNode] =
      positionedNode((functionType | elmTypeExceptFunction) <~ opt(comment) ^^ {
        typ =>
          SyntaxNode.parent(name, Seq(typ))
      })

    def elmTypeExceptFunction: Parser[PositionedSyntaxNode] =
      typeReference |
        variableTypeReference |
        recordType |
        tupleType |
        parensAroundType |
        unit |
        hint("an Elm Type")

    private def unit = positionedNode("()" ^^ SyntaxNode.leaf("unitType"))

    private def parensAroundType: Parser[PositionedSyntaxNode] = "(" ~> elmType("insideParens") <~ opt("moveLeft") ~ ")"

    private def functionType: Parser[PositionedSyntaxNode] =
      positionedNode(rep1sep(elmTypeExceptFunction, "->") ^^ {
        case one if one.length == 1 => SyntaxNode.unposition(one.head)
        case more => SyntaxNode.parent("functionType", more)
      })

    private def typeReference: Parser[PositionedSyntaxNode] =
      positionedNode(qualifiedUppercaseIdentifier("typeName") ~ rep(elmType("typeParameter")) ^^ {
        case typeName ~ parameters =>
          SyntaxNode.parent("typeReference", typeName +: parameters)
      })

    private def variableTypeReference = lowercaseIdentifier("typeVariable")

    private def tupleType: Parser[PositionedSyntaxNode] =
      positionedNode("(" ~> rep1sep(elmType("tupleTypePart"), commaSeparator) <~ opt("moveLeft") ~ ")" ^^ { parts =>
        SyntaxNode.parent("tupleType", parts)
      })

    private def recordTypeFieldDeclaration: Parser[PositionedSyntaxNode] =
      positionedNode(lowercaseIdentifier("fieldName") ~ ":" ~ elmType("fieldType") ^^ {
        case name ~ _ ~ typ => SyntaxNode.parent("recordTypeField", Seq(name, typ))
      })

    private def recordType: Parser[PositionedSyntaxNode] =
      positionedNode("{" ~> repsep(recordTypeFieldDeclaration, commaSeparator) <~ opt("moveLeft") ~ "}" ^^ {
        fields => SyntaxNode.parent("recordType", fields)
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

  /* for testing */
  def parsePart(parser: Parser[PositionedSyntaxNode], input: String) = {
    parse(parser, input) match {
      case Success(result, next) => result
      case boo: NoSuccess => throw new RuntimeException(boo.toString)
    }
  }

  def positionedNode(inner: Parser[SyntaxNode], label: Option[String] = None) = new Parser[PositionedSyntaxNode] {
    override def apply(in: Input): ParseResult[PositionedSyntaxNode] = {
      val start = handleWhiteSpace(in.source, in.offset)
    //  println("Trying to parse: " + in.source.subSequence(in.offset, Math.min(in.source.length(), in.offset + 10 )))
      inner.apply(in) match {
        case Error(msg, next) => Error(msg, next)
        case Failure(msg, next) =>
          // label.foreach(l => println(s"Failed to match on ${label} at [${in.pos.line},${in.pos.column}]"))
          Failure(msg, next)
        case Success(result, next) =>
          //  println(s"Positioning match of ${result.name} at [${in.pos.line},${in.pos.column}]-[${next.pos.line},${next.pos.column}]")
          val end = next.offset
          Success(PositionedSyntaxNode(result.name, result.childNodes, result.valueOption,
            start, end), next)
      }
    }
  }

  def wrap(name: String, inner: Parser[PositionedSyntaxNode]): Parser[PositionedSyntaxNode] =
    inner ^^ { i => PositionedSyntaxNode(name, Seq(i), i.valueOption, i.startOffset, i.endOffset) }

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
    SyntaxNode(positionedSyntaxNode.nodeName, positionedSyntaxNode.parsedNodes, positionedSyntaxNode.valueOption)
  }
}

case class PositionedSyntaxNode(override val nodeName: String,
                                override val parsedNodes: Seq[PositionedSyntaxNode],
                                valueOption: Option[String],
                                override val startOffset: Int,
                                override val endOffset: Int) extends ParsedNode {
  def values: String = valueOption.getOrElse(parsedNodes.map(_.values).mkString(" "))

  def chomp: PositionedSyntaxNode =
    valueOption match {
      case Some(stringWithNewline) if stringWithNewline.endsWith("\n") =>
        PositionedSyntaxNode(nodeName,
          parsedNodes,
          Some(stringWithNewline.substring(0, stringWithNewline.length - 1)),
          startOffset,
          endOffset - 1)
      case _ => this
    }
}