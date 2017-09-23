package ast

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

class ASTList(_list: List[ASTree]) extends ASTree {

  protected val children: List[ASTree] = _list

  override def child(i: Int): Option[ASTree] = Option(children.apply(i))

  override def numberOfChildren(): Int = children.size

  override def into_iter(): Iterator[ASTree] = children.iterator

  override def location: String = {
    children.map(f => f.location).filter(s => !s.isEmpty).head
  }
}
