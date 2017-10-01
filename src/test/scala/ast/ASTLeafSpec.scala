package ast

import ast.ASTLeaf.{Name, NumberLiteral, StringLiteral}
import environment.{NameEnvironment, NullEnvironment}
import org.scalatest.WordSpec
import token.{NumberToken, StringToken}

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

class ASTLeafSpec extends WordSpec {

  "ASTLeaf" should {
    "return empty value" in {
      assert(ASTLeaf.EMPTY == List.empty)
    }

    "child method will return None value" in {
      val leaf = new ASTLeaf(StringToken(1, ""))
      assert(leaf.child(0).isEmpty)
    }

    "numberOfChildren method will return 0" in {
      val leaf = new ASTLeaf(StringToken(1, ""))
      assert(leaf.numberOfChildren() == 0)
    }

    "toString method will return some appropriate words" in {
      val leaf = new ASTLeaf(StringToken(1, "appropriate value"))
      assert(leaf.toString() == "appropriate value")
    }

    "return the appropriate token text" in {
      val leaf = new ASTLeaf(StringToken(1, "string token"))
      assert(leaf.toString() == "string token")
    }

    "return the appropriate location" in {
      val leaf = new ASTLeaf(StringToken(1, "string token"))
      assert(leaf.location == "at line 1")
    }

    "return the appropriate token" in {
      val leaf = new ASTLeaf(NumberToken(1, 1))
      assert(leaf.toToken() == NumberToken(1, 1))
    }

    "eval method will return DiamondException" in {
      val leaf = new ASTLeaf(StringToken(1, ""))
      assert(leaf.eval(new NullEnvironment).isLeft)
    }
  }

  "Name" should {
    "return its appropriate name" in {
      val name = Name(StringToken(1, "name"))
      assert(name.name() == "name")
    }

    "eval name and return name string" in {
      val name = Name(StringToken(1, "name"))
      val env = new NameEnvironment
      env.put("name", name)

      assert(String.valueOf(name.eval(env).right.get.get) == "name")
    }
  }

  "NumberLiteral" should {
    "return its appropriate value" in {
      val numberLiteral = NumberLiteral(NumberToken(1, 999))
      assert(numberLiteral.value() == 999)
    }

    "eval number and return number value" in {
      val num = NumberLiteral(NumberToken(1, 1))
      assert(num.eval(new NullEnvironment).right.get.get == 1)
    }
  }

  "StringLiteral" should {
    "return its appropriate value" in {
      val stringLiteral = StringLiteral(StringToken(1, "aaa"))
      assert(stringLiteral.value() == "aaa")
    }

    "eval string and return string value" in {
      val string = StringLiteral(StringToken(1, "aaa"))
      assert(string.eval(new NullEnvironment).right.get.get == "aaa")
    }
  }
}
