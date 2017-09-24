package ast

import org.scalatest.WordSpec
import token.{IdentifierToken, NumberToken}

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
      val actual = new ASTList(
        List[ASTree](NumberLiteral(NumberToken(1, 1)),
                     NumberLiteral(NumberToken(1, 2)),
                     NumberLiteral(NumberToken(1, 3))))
      val expected = NumberLiteral(NumberToken(1, 1))
      assert(actual.child(0).get == expected)
    }

    "has appropriate children size in simple pattern" in {
      val actual = new ASTList(
        List[ASTree](NumberLiteral(NumberToken(1, 1)),
                     NumberLiteral(NumberToken(1, 2)),
                     NumberLiteral(NumberToken(1, 3))))
      val expected = 3

      assert(actual.numberOfChildren() == expected)
    }

    "has appropriate children size in nested pattern" in {
      // (1 + 2) + 3
      val actual = new ASTList(
        List[ASTree](
          PrimaryExpr(List[ASTree](Name(IdentifierToken(2, "(")))),
          NumberLiteral(NumberToken(1, 1)),
          PrimaryExpr(List[ASTree](Name(IdentifierToken(1, "+")))),
          NumberLiteral(NumberToken(1, 2)),
          PrimaryExpr(List[ASTree](Name(IdentifierToken(1, ")")))),
          PrimaryExpr(List[ASTree](Name(IdentifierToken(1, "+")))),
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
}
