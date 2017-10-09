package parser

import ast.ASTLeaf.Name
import ast.ASTList.PrimaryExpr
import ast._
import lexer.Lexer
import parser.GenericParser.program
import parser.Parser.Operators
import parser.exception.ParseException
import token.Token

/*
 * Copyright 2017 Yuki Toyoda
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

object GenericParser {

  private val reserved: Set[String] = Set(";", "}", Token.EOL)

  private val operators: Operators = new Operators

  {
    operators.add("=", 1, Operators.RIGHT)
    operators.add("==", 2, Operators.LEFT)
    operators.add(">", 2, Operators.LEFT)
    operators.add("<", 2, Operators.LEFT)
    operators.add("+", 3, Operators.LEFT)
    operators.add("-", 3, Operators.LEFT)
    operators.add("*", 4, Operators.LEFT)
    operators.add("/", 4, Operators.LEFT)
    operators.add("%", 4, Operators.LEFT)
  }

  private val program: Parser = assembleParser()

  private def assembleParser(): Parser = {
    import collection.JavaConverters._

    type FromASTList = Class[_ <: ASTList]

    val expr0 = Parser.rule
    val primary = Parser
      .rule(classOf[PrimaryExpr])
      .or(
        Parser.rule.sep("(").ast(expr0).sep(")"),
        Parser.rule.number(classOf[ASTLeaf.NumberLiteral]),
        Parser.rule.identifier(classOf[ASTLeaf.Name], reserved.asJava),
        Parser.rule.string(classOf[ASTLeaf.StringLiteral])
      )
    val factor = Parser.rule.or(
      Parser.rule(classOf[ASTList.NegativeExpr]).sep("-").ast(primary),
      primary)
    val expr = expr0.expression(classOf[ASTList.BinaryExpr], factor, operators)

    val statement0 = Parser.rule
    val block = Parser
      .rule(classOf[ASTList.BlockStatement])
      .sep("{")
      .option(statement0)
      .repeat(Parser.rule.sep(";", Token.EOL).option(statement0))
      .sep("}")
    val simple = Parser.rule(classOf[PrimaryExpr]).ast(expr)
    val statement = statement0.or(
      Parser
        .rule(classOf[ASTList.IfStatement])
        .sep("if")
        .ast(expr)
        .ast(block)
        .option(Parser.rule.sep("else").ast(block)),
      Parser.rule(classOf[ASTList.WhileStatement]).sep("while").ast(expr).ast(block),
      simple
    )

    val program = Parser.rule
      .or(statement, Parser.rule(classOf[ASTList.NullStatement]))
      .sep(";", Token.EOL)

    program
  }
}

class GenericParser {

  def parse(lexer: Lexer): Either[ParseException, ASTree] = {
    try {
      Right(program.parse(lexer))
    } catch {
      case parseException: ParseException => Left(parseException)
    }
  }
}
