package parser

import java.io.{InputStream, InputStreamReader, LineNumberReader}

import lexer.Lexer
import org.scalatest.WordSpec
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

class GenericParserSpec extends AbstractParserSpec {

  "parse" should {
    "return appropriate result" in {
      val snatcher = createNewSnatcher()
      val code = "sum = (1 + 2) + 3;\n" + "sum;"
      snatcher.input(code)
      val lexer =
        new Lexer(new LineNumberReader(new InputStreamReader(snatcher)))

      while (lexer.peek(0) != Token.EOF) {
        val ast = new GenericParser().parse(lexer)
        println(ast.getOrElse())
      }
    }

    "return block statement" in {
      val snatcher = createNewSnatcher()
      val code = new StringBuilder()
        .append("if i % 2 == 0 {\n")
        .append("even = even + 1\n")
        .append("};")
      snatcher.input(code.toString())
      val lexer =
        new Lexer(new LineNumberReader(new InputStreamReader(snatcher)))

      while (lexer.peek(0) != Token.EOF) {
        new GenericParser().parse(lexer) match {
          case Left(e) => {
            throw new IllegalStateException(e.getMessage)
          }
          case Right(ast) => {
            println(ast.toString())
          }
        }
      }
    }
  }
}
