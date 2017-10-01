package ast

import ast.ASTLeaf.{Name, NumberLiteral}
import ast.ASTList._
import environment.NullEnvironment
import org.scalatest.WordSpec
import token.{IdentifierToken, NumberToken, StringToken}

import scala.collection.JavaConverters._

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

class ASTListSpec extends WordSpec {

  "ASTList" should {
    "return its appropriate child" in {
      val actual = ASTList.newInstance(
        List[ASTree](NumberLiteral(NumberToken(1, 1)),
                     NumberLiteral(NumberToken(1, 2)),
                     NumberLiteral(NumberToken(1, 3))))
      val expected = NumberLiteral(NumberToken(1, 1))
      assert(actual.child(0).get == expected)
    }

    "has appropriate children size in simple pattern" in {
      val actual = ASTList.newInstance(
        List[ASTree](NumberLiteral(NumberToken(1, 1)),
                     NumberLiteral(NumberToken(1, 2)),
                     NumberLiteral(NumberToken(1, 3))))
      val expected = 3

      assert(actual.numberOfChildren() == expected)
    }

    "has appropriate children size in nested pattern" in {
      // (1 + 2) + 3
      val actual = ASTList.newInstance(
        List[ASTree](
          PrimaryExpr.newInstance(List[ASTree](Name(IdentifierToken(2, "(")))),
          NumberLiteral(NumberToken(1, 1)),
          PrimaryExpr.newInstance(List[ASTree](Name(IdentifierToken(1, "+")))),
          NumberLiteral(NumberToken(1, 2)),
          PrimaryExpr.newInstance(List[ASTree](Name(IdentifierToken(1, ")")))),
          PrimaryExpr.newInstance(List[ASTree](Name(IdentifierToken(1, "+")))),
          NumberLiteral(NumberToken(1, 3))
        ))

      {
        val expected = 7
        assert(actual.numberOfChildren() == expected)
      }

      {
        val expected = "at line 2"
        assert(actual.location == expected)
      }
    }
  }

  "NegativeExpr" should {
    "has appropriate value" in {
      val expr =
        NegativeExpr(List[ASTree](Name(IdentifierToken(1, "*"))).asJava)

      {
        val actual = expr.operand()
        val expected = Name(IdentifierToken(1, "*"))
        assert(actual == expected)
      }

      {
        val actual = expr.toString()
        val expected = "-*"
        assert(actual == expected)
      }

      {
        assert(expr.eval(new NullEnvironment()).isLeft)
      }
    }
  }

  "IfStatement" should {
    "return appropriate value" in {
      val expr = IfStatement(
        List[ASTree](Name(StringToken(1, "condition")),
                     Name(StringToken(1, "thenBlock")),
                     Name(StringToken(1, "elseBlock"))).asJava)

      assert(expr.condition() == Name(StringToken(1, "condition")))
      assert(expr.thenBlock() == Name(StringToken(1, "thenBlock")))
      assert(expr.elseBlock().get == Name(StringToken(1, "elseBlock")))

      assert(expr.toString() == "(if condition thenBlock else elseBlock)")
    }
  }

  "WhileStatement" should {
    "return appropriate value" in {
      val expr = WhileStatement(
        List[ASTree](Name(StringToken(1, "condition")),
                     Name(StringToken(1, "body"))).asJava)

      assert(expr.condition() == Name(StringToken(1, "condition")))
      assert(expr.body() == Name(StringToken(1, "body")))

      assert(expr.toString() == "(while condition body)")
    }
  }

  "NullStatement" should {
    "return exception if eval method is called" in {
      val expr = NullStatement(List[ASTree]().asJava)
      assert(expr.eval(new NullEnvironment()).isLeft)
    }
  }
}
