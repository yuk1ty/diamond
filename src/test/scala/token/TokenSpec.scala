package token

import org.scalatest.WordSpec

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

class TokenSpec extends WordSpec {

  "NumberToken" should {
    "return line value of 100" in {
      val token = new NumberToken(100, 1)
      assert(token.lineNumber == 100)
    }

    "return value value of 100" in {
      val token = new NumberToken(1, 100)
      assert(token.getValue == 100)
    }

    "return isNumber to be true" in {
      val token = new NumberToken(1, 1)
      assert(token.isNumber)
    }
  }

  "StringToken" should {
    "return line value of 100" in {
      val token = new StringToken(100, "")
      assert(token.lineNumber == 100)
    }

    "return literal value of \"AAA\"" in {
      val token = new StringToken(100, "AAA")
      assert(token.getText == "AAA")
    }

    "return isString to be true" in {
      val token = new StringToken(1, "")
      assert(token.isString)
    }
  }

  "IdentifierToken" should {
    "return line value of 100" in {
      val token = new IdentifierToken(100, "")
      assert(token.lineNumber == 100)
    }

    "return id value of a123bcd" in {
      val token = new IdentifierToken(1, "a123bcd")
      assert(token.getText == "a123bcd")
    }

    "return isIdentifier to be true" in {
      val token = new IdentifierToken(1, "")
      assert(token.isIdentifier)
    }
  }
}
