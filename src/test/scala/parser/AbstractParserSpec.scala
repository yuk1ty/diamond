package parser

import java.io.InputStream

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

class AbstractParserSpec extends WordSpec {

  protected class StandardInputSnatcher extends InputStream {

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

  protected def createNewSnatcher(): StandardInputSnatcher = {
    new StandardInputSnatcher
  }
}
