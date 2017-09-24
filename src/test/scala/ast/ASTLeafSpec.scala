package ast

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

  "ASLeaf" should {
    "return the appropriate token text" in {
      val leaf = new ASTLeaf(StringToken(1, "string token"))
      assert(leaf.toString() == "string token")
    }

    "return the appropriate location" in {
      val leaf = new ASTLeaf(StringToken(1, "string token"))
      assert(leaf.location == "at line 1")
    }
  }

  "Name" should {
    "return its appropriate name" in {
      val name = Name(StringToken(1, "name"))
      assert(name.name() == "name")
    }
  }

  "NumberLiteral" should {
    "return its appropriate value" in {
      val numberLiteral = NumberLiteral(NumberToken(1, 999))
      assert(numberLiteral.value() == 999)
    }
  }
}
