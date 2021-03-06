package lexer

import java.io.{LineNumberReader, Reader}
import java.util.regex.{Matcher, Pattern}

import token.{IdentifierToken, NumberToken, StringToken, Token}

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

object Lexer {

  val REGEX_PATTERN: String =
    "\\s*((//.*)|([0-9]+)|(\"(\\\\\"|\\\\\\\\|\\\\n|[^\"])*\")".+(
      "|[A-Z_a-z][A-Z_a-z0-9]*|==|<=|>=|&&|\\|\\||\\p{Punct})?")
}

class Lexer(_reader: Reader) {

  val pattern: Pattern = Pattern.compile(Lexer.REGEX_PATTERN)

  val queue: ListBuffer[Token] = ListBuffer()

  var hasMore: Boolean = true

  var reader: LineNumberReader = new LineNumberReader(_reader)

  def read: Token = {
    if (fillQueue(0)) {
      queue.remove(0)
    } else {
      Token.EOF
    }
  }

  def peek(i: Int): Token = {
    if (fillQueue(i)) {
      queue.apply(i)
    } else {
      Token.EOF
    }
  }

  private def fillQueue(i: Int): Boolean = {
    while (i >= queue.size) {
      if (hasMore) {
        readLine()
      } else {
        return false
      }
    }
    true
  }

  protected def readLine(): Unit = {
    val memorized: Either[Throwable, Option[String]] = try {
      Right(Option(reader.readLine()))
    } catch {
      case e: Throwable => Left(e)
    }

    memorized match {
      case Left(e) => e.printStackTrace()
      case Right(op) =>
        op match {
          case Some(value) => innerReadLine(value)
          case None        => hasMore = false
        }
    }
  }

  private def innerReadLine(value: String): Unit = {
    val lineNo = reader.getLineNumber
    val matcher = pattern.matcher(value)
    matcher.useTransparentBounds(true).useAnchoringBounds(false)

    var position = 0
    val endPoint = value.length

    while (position < endPoint) {
      matcher.region(position, endPoint)

      if (matcher.lookingAt) {
        addToken(lineNo, matcher)
        position = matcher.end
      }
    }
  }

  protected def addToken(lineNo: Int, matcher: Matcher): Unit = {
    toOption(matcher.group(1)) match {
      case Some(m) =>
        toOption(matcher.group(2)) match {
          case None =>
            val tk = if (toOption(matcher.group(3)).nonEmpty) {
              NumberToken(lineNo, m.toInt)
            } else if (toOption(matcher.group(4)).nonEmpty) {
              StringToken(lineNo, toStringLiteral(m))
            } else {
              IdentifierToken(lineNo, m)
            }
            queue.+=(tk)
        }
    }

    def toOption(result: String): Option[String] = {
      Option(result)
    }

    def toStringLiteral(text: String): String = {
      val sb: StringBuilder = new StringBuilder
      val len: Int = text.length - 1

      for (i <- 1 until len) {
        var c = text.charAt(i)

        if (c == '\\' && i + 1 < len) {
          val next = text.charAt(i + 1)
          if (next == '"' || next == '\\') {
            c = text.charAt(i.+(1))
          } else if (c == 'n') {
            // TODO ++i
            i.+(1)
            c = '\n'
          }
        }

        sb.append(c)
      }

      sb.toString()
    }
  }
}
