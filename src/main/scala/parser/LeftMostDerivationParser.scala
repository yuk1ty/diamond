package parser

import ast.ASTLeaf.NumberLiteral
import ast.ASTList.BinaryExpr
import ast.{ASTLeaf, ASTree}
import lexer.Lexer
import parser.exception.ParseException

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

class LeftMostDerivationParser(_lexer: Lexer) {

  def expression(): Either[ParseException, ASTree] = {
    var left = term() match {
      case Left(e)  => return Left(e)
      case Right(r) => r
    }

    while (isToken("+") || isToken("-")) {
      val op = new ASTLeaf(_lexer.read)
      val right = term() match {
        case Left(e)  => return Left(e)
        case Right(r) => r
      }

      left = BinaryExpr.newInstance(List[ASTree](left, op, right))
    }

    Right(left)
  }

  private def term(): Either[ParseException, ASTree] = {
    var left = factor() match {
      case Left(e)  => return Left(e)
      case Right(r) => r
    }

    while (isToken("*") || isToken("/")) {
      val op = new ASTLeaf(_lexer.read)
      val right = factor() match {
        case Left(e)  => return Left(e)
        case Right(r) => r
      }

      left = BinaryExpr.newInstance(List[ASTree](left, op, right))
    }

    Right(left)
  }

  private def factor(): Either[ParseException, ASTree] = {
    if (isToken("(")) {
      readTokenAndProceed("(")
      val result = expression()
      readTokenAndProceed(")")

      result
    } else {
      val token = _lexer.read

      if (token.isNumber) {
        val numberLiteral = NumberLiteral(token)

        Right(numberLiteral)
      } else {
        Left(new ParseException(token))
      }
    }
  }

  private def readTokenAndProceed(
      name: String): Either[ParseException, Unit] = {
    val token = _lexer.read

    if (!(token.isIdentifier && name == token.getText))
      return Left(new ParseException(token))

    Right(Unit)
  }

  private def isToken(name: String): Boolean = {
    val token = _lexer.peek(0)

    token.isIdentifier && name == token.getText
  }
}
