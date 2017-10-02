package interpreter

import java.io.{InputStream, InputStreamReader, LineNumberReader}

import environment.NameEnvironment
import lexer.Lexer
import org.scalatest.WordSpec
import parser.GenericParser

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

class GenericInterpreterSpec extends WordSpec {

  "GenericInterpreterSpec" should {
    class StandardInputSnatcher extends InputStream {

      private val buffer = new StringBuilder

      def input(in: String): Unit = {
        buffer.append(in).append(System.getProperty("line.separator"))
      }

      override def read(): Int = {
        if (buffer.isEmpty) {
          return -1
        }

        val result = buffer.charAt(0)
        buffer.deleteCharAt(0)

        result
      }
    }

    "summing up 1 + 2 + 3 = 5" in {
      val snatcher = new StandardInputSnatcher
      val code = "sum = (1 + 2) + 3\n" + "sum\n"
      snatcher.input(code)
      val lexer =
        new Lexer(new LineNumberReader(new InputStreamReader(snatcher)))
      GenericInterpreter.run(new GenericParser, new NameEnvironment, lexer)
    }
  }
}
