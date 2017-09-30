package environment

import scala.collection.mutable

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

abstract class AbstractEnvironment[T] extends Environment[T] {

  private val repository = mutable.Map[TypeSafePropertyKey[T], T]()

  def put(key: TypeSafePropertyKey[T], instance: T): Unit = repository += (key -> instance)

  def get(key: TypeSafePropertyKey[T]): Option[T] = repository.get(key)
}