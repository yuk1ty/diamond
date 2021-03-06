package environment

import ast.ASTLeaf.Name
import org.scalatest.WordSpec
import token.StringToken

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

class EnvironmentSpec extends WordSpec {

  "NameEnvironment" should {
    "get its appropriate value" in {
      val environment = new NameEnvironment()

      environment.put("a", Name(StringToken(1, "a")))
      environment.put("b", Name(StringToken(1, "b")))
      environment.put("c", Name(StringToken(1, "c")))

      assert(environment.get("a").get == Name(StringToken(1, "a")))
      assert(environment.get("b").get == Name(StringToken(1, "b")))
      assert(environment.get("c").get == Name(StringToken(1, "c")))
    }
  }
}
