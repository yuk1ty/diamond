package lexer

import java.io.{InputStream, InputStreamReader, LineNumberReader}

import org.scalatest.PrivateMethodTester._
import org.scalatest.WordSpec
import token.Token

import scala.collection.mutable.ListBuffer

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

class LexerSpec extends WordSpec {

  "innerReadLine" should {
    "create NumberToken which has a value of 2" in {
      val clazz =
        new Lexer(new LineNumberReader(new InputStreamReader(System.in)))
      val methodName = PrivateMethod[Unit]('innerReadLine)

      val code = "2"

      clazz invokePrivate methodName(code)

      val actualToken = clazz.read
      val actual = actualToken.getValue
      val expected = 2

      assert(actualToken.isNumber)
      assert(actual == expected)
    }

    "create IdentifierToken which has a value of variable a" in {
      val clazz =
        new Lexer(new LineNumberReader(new InputStreamReader(System.in)))
      val methodName = PrivateMethod[Unit]('innerReadLine)

      val variableCode = "variable"

      clazz invokePrivate methodName(variableCode)

      val actualToken = clazz.read
      val actual = actualToken.getText
      val expected = "variable"

      assert(actualToken.isIdentifier)
      assert(actual == expected)
    }

    "create IdentifierToken which has a value of \"{\"" in {
      val clazz =
        new Lexer(new LineNumberReader(new InputStreamReader(System.in)))
      val methodName = PrivateMethod[Unit]('innerReadLine)

      val openCode = "{"

      clazz invokePrivate methodName(openCode)

      val actualToken = clazz.read
      val actual = actualToken.getText
      val expected = "{"

      assert(actualToken.isIdentifier)
      assert(actual == expected)
    }

    "create StringToken which has a value of \"scala\"" in {
      val clazz =
        new Lexer(new LineNumberReader(new InputStreamReader(System.in)))
      val methodName = PrivateMethod[Unit]('innerReadLine)

      val text = "\"scala\""

      clazz invokePrivate methodName(text)

      val actualToken = clazz.read
      val actual = actualToken.getText
      val expected = "scala"

      assert(actualToken.isString)
      assert(actual == expected)
    }
  }

  "read" should {
    class StandardInputSnatcher extends InputStream {

      private var buffer = new StringBuilder

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

    object LexerRunner {

      def tokens(lexer: Lexer): List[Token] = {
        var end = true
        val tokens = ListBuffer[Token]()

        while (end) {
          val token = lexer.read
          if (token == Token.EOF) {
            end = false
          } else {
            tokens += token
          }
        }

        tokens.toList
      }
    }

    "interpret simple formula" ignore {
      val snatcher = new StandardInputSnatcher
      snatcher.input("a = 1 + 2;")

      val clazz =
        new Lexer(new LineNumberReader(new InputStreamReader(snatcher)))
      val tokens = LexerRunner.tokens(clazz)
      tokens.foreach(println)
    }

    "interpret if clause" in {
      val snatcher = new StandardInputSnatcher
      val code = "if (flag) {"
        .concat("a = 1 + 2")
        .concat("} else {")
        .concat("b = 1")
        .concat("}")
      snatcher.input(code)

      val clazz =
        new Lexer(new LineNumberReader(new InputStreamReader(snatcher)))
      val tokens = LexerRunner.tokens(clazz)
      tokens.foreach(println)
    }
  }
}
