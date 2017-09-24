package parser

import ast._
import lexer.Lexer
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

object ParserFactory {

  private val reversed: Set[String] = Set(";", "}", Token.EOL)

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
    type FromASTLeaf = Class[_ <: ASTLeaf]

    def rule(): Parser = {
      Parser.rule()
    }

    val expr0 = rule()
    val primary = Parser
      .rule(PrimaryExpr.getClass.asInstanceOf[FromASTList])
      .or(
        rule().sep("(").ast(expr0).sep(")"),
        rule().number(NumberLiteral.getClass.asInstanceOf[FromASTLeaf]),
        rule().identifier(Name.getClass.asInstanceOf[FromASTLeaf],
                          reversed.asJava),
        rule().string(StringLiteral.getClass.asInstanceOf[FromASTLeaf])
      )
    val factor = rule().or(
      Parser
        .rule(NegativeExpr.getClass.asInstanceOf[FromASTList])
        .sep("-")
        .ast(primary),
      primary)
    val expr = expr0.expression(BinaryExpr.getClass.asInstanceOf[FromASTList],
                                factor,
                                operators)

    val statement0 = rule()
    val block = Parser
      .rule(BlockStatement.getClass.asInstanceOf[FromASTList])
      .sep("{")
      .option(statement0)
      .repeat(rule().sep(";", Token.EOL).option(statement0))
      .sep("}")
    val simple =
      Parser.rule(PrimaryExpr.getClass.asInstanceOf[FromASTList]).ast(expr)
    val statement = statement0.or(
      Parser
        .rule(IfStatement.getClass.asInstanceOf[FromASTList])
        .sep("if")
        .ast(expr)
        .ast(block)
        .option(rule().sep("else").ast(block)),
      Parser
        .rule(WhileStatement.getClass.asInstanceOf[FromASTList])
        .sep("while")
        .ast(expr)
        .ast(block),
      simple
    )

    val program = rule().or(
      statement,
      Parser
        .rule(NullStatement.getClass.asInstanceOf[FromASTList])
        .sep(";", Token.EOL))

    program
  }

  def parse(lexer: Lexer): Either[ParseException, ASTree] = {
    try {
      Right(program.parse(lexer))
    } catch {
      case parseException: ParseException => Left(parseException)
    }
  }
}
