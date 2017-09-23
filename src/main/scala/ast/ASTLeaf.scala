package ast

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

object ASTLeaf {

  val EMPTY: List[ASTree] = List[ASTree]()
}

class ASTLeaf(_token: Token) extends ASTree {

  protected val token: Token = _token

  override def child(i: Int): Option[ASTree] = None

  override def numberOfChildren(): Int = 0

  override def into_iter(): Iterator[ASTree] = ASTLeaf.EMPTY.iterator

  override def toString(): String = token.getText

  override def location: String = "at line ".concat(token.lineNumber.toString)

  def toToken(): Token = token
}