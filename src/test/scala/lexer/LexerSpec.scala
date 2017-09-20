package lexer

import java.io.{InputStreamReader, LineNumberReader}

import org.scalatest.PrivateMethodTester._
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

class LexerSpec extends WordSpec {

  "innerReadLine" should {
    "create NumberToken which has a value of 2" in {
      val clazz =
        new Lexer(new LineNumberReader(new InputStreamReader(System.in)))
      val methodName = PrivateMethod[Unit]('innerReadLine)

      val code = "2"

      // 一度メソッドを呼んで値を保持させる
      clazz invokePrivate methodName(code)

      val actualToken = clazz.read
      val actual = actualToken.getValue
      val expected = 2

      assert(actualToken.isNumber)
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

    // TODO this case will fail because toStringLiteral is looping
    "create StringToken which has a value of \"scala\"" in {
      val clazz = new Lexer(new LineNumberReader(new InputStreamReader(System.in)))
      val methodName = PrivateMethod[Unit]('innerReadLine)

      val text = "\"scala\""

      val actualToken = clazz.read
      val actual = actualToken.getText
      val expected = "scala"

      assert(actualToken.isString)
      assert(actual == expected)
    }
  }
}
