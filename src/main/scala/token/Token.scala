package token

import ast.ASTree
import exception.DiamondException

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

object Token {

  val EOF: Token = new Token(-1) {}

  val EOL: String = "\\n"
}

sealed abstract class Token(_line: Int) {

  val lineNumber: Int = _line

  def isIdentifier: Boolean = false

  def isNumber: Boolean = false

  def isString: Boolean = false

  def getValue: Int = 0

  def getText: String = ""
}

case class NumberToken(_line: Int, _value: Int) extends Token(_line) {

  val value: Int = _value

  override def isNumber: Boolean = true

  override def getText: String = value.toString

  override def getValue: Int = value
}

case class IdentifierToken(_line: Int, _id: String) extends Token(_line) {

  val text: String = _id

  override def isIdentifier: Boolean = true

  override def getText: String = text
}

case class StringToken(_line: Int, _literal: String) extends Token(_line) {

  val literal: String = _literal

  override def isString: Boolean = true

  override def getText: String = literal
}
