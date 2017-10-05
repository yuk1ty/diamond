package parser

import java.io.{InputStreamReader, LineNumberReader}

import lexer.Lexer
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

class LeftMostDerivationParserSpec extends AbstractParserSpec {

  "expression" should {
    "parse approperiate abstraction tree" in {
      val snatcher = createNewSnatcher()
      val code = new StringBuilder()
        .append("1 + 2\n")
      snatcher.input(code.toString())
      val lexer =
        new Lexer(new LineNumberReader(new InputStreamReader(snatcher)))

      val llParser = new LeftMostDerivationParser(lexer)
      val result = llParser.expression()
      println("=> " + result)
    }
  }
}
