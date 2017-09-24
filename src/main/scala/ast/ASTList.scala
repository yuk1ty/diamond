package ast

import java.util.StringJoiner

import scala.collection.JavaConverters._

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

object ASTList {

  def fromJavaApi(list: java.util.List[ASTree]): ASTList = {
    new ASTList(list.asScala.toList)
  }
}

sealed class ASTList(_list: List[ASTree]) extends ASTree {

  protected val children: List[ASTree] = _list

  override def child(i: Int): Option[ASTree] = Option(children.apply(i))

  override def numberOfChildren(): Int = children.size

  override def into_iter(): Iterator[ASTree] = children.iterator

  override def location: String = {
    children.map(f => f.location).filter(s => !s.isEmpty).head
  }

  override def toString(): String = {
    val joiner = new StringJoiner(" ")
    children.map(e => e.toString()).foreach(_ => joiner.add(_))
    joiner.toString
  }
}

// BinaryExpr

case class BinaryExpr(_list: List[ASTree]) extends ASTList(_list) {

  def left(): ASTree = child(0).get

  def operator(): String = child(1).asInstanceOf[ASTLeaf].toToken().getText

  def right(): ASTree = child(2).get
}

// PrimaryExpr

object PrimaryExpr {

  def create(c: List[ASTree]): ASTree = {
    c.size match {
      case 1 => c.head
      case _ => new PrimaryExpr(c)
    }
  }
}

case class PrimaryExpr(_c: List[ASTree]) extends ASTList(_c) {}

// NegativeExpr

case class NegativeExpr(_c: List[ASTree]) extends ASTList(_c) {

  def operand(): ASTree = child(0).get

  override def toString(): String = "-" + operand()
}

// BlockStatement

case class BlockStatement(_c: List[ASTree]) extends ASTList(_c) {}

// IfStatement

case class IfStatement(_c: List[ASTree]) extends ASTList(_c) {

  def condition(): ASTree = child(0).get

  def thenBlock(): ASTree = child(1).get

  def elseBlock(): Option[ASTree] =
    if (numberOfChildren() > 2) Some(child(2).get) else None

  override def toString(): String =
    "(if " + condition() + " " + thenBlock() + " else " + elseBlock() + ")"
}

// WhileStatement

case class WhileStatement(_c: List[ASTree]) extends ASTList(_c) {

  def condition(): ASTree = child(0).get

  def body(): ASTree = child(1).get

  override def toString(): String = "(while " + condition() + " " + body() + ")"
}

// NullStatement

case class NullStatement(_c: List[ASTree]) extends ASTList(_c) {}
