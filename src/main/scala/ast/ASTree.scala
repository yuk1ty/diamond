package ast

import environment.Environment
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

trait ASTree extends Iterable[ASTree] {

  def child(i: Int): Option[ASTree]

  def numberOfChildren(): Int

  def into_iter(): Iterator[ASTree]

  def location: String

  def iterator: Iterator[ASTree] = into_iter()

  // Parser.java is not supported for Generics,
  // so the return value of this def is Any
  def eval(env: Environment): Either[DiamondException, Option[Any]]
}
