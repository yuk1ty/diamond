package interpreter

import java.io.{BufferedReader, InputStreamReader}

import ast.ASTList.NullStatement
import environment.Environment
import lexer.Lexer
import parser.GenericParser
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

object GenericInterpreter {

  def run(parser: GenericParser, env: Environment, lexer: Lexer): Unit = {
    while (lexer.peek(0) != Token.EOF) {
      parser.parse(lexer) match {
        case Left(e) => e.printStackTrace()
        case Right(tree) =>
          if (!tree.isInstanceOf[NullStatement]) {
            tree.eval(env) match {
              case Left(e)       => e.printStackTrace()
              case Right(result) => println("=> " + result)
            }
          }
      }
    }
  }
}
