package ast

import java.util.StringJoiner

import ast.ASTLeaf.Name
import environment.Environment
import exception.DiamondException

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

  private[this] val EMPTY: String = ""

  private[this] val EQ: String = "="

  private[this] val PLUS: String = "+"

  private[this] val MINUS: String = "-"

  private[this] val MUL: String = "*"

  private[this] val DIV: String = "/"

  private[this] val RES: String = "%"

  private[this] val TRUE: Int = 1

  private[this] val FALSE: Int = 0

  private[this] val GT: String = ">"

  private[this] val LT: String = "<"

  def newInstance(list: List[ASTree]): ASTList = {
    new ASTList(list.asJava)
  }

  def newInstance(list: java.util.List[ASTree]): ASTList = {
    new ASTList(list)
  }

  // BinaryExpr

  object BinaryExpr {

    def newInstance(c: List[ASTree]): ASTree = new BinaryExpr(c.asJava)
  }

  case class BinaryExpr(_list: java.util.List[ASTree]) extends ASTList(_list) {

    def left(): ASTree = child(0).get

    def operator(): String = child(1).asInstanceOf[ASTLeaf].toToken().getText

    def right(): ASTree = child(2).get

    override def eval(
        env: Environment): Either[DiamondException, Option[Any]] = {
      if (operator() == EQ) {
        right().eval(env) match {
          case Right(rhs) =>
            rhs match {
              case Some(rhs2) =>
                computeAssign(env, rhs2) match {
                  case Left(e) =>
                    Left(new DiamondException(e.getMessage, right()))
                  case Right(value: Any) => Right(Option(value))
                }
              case None =>
                Left(new DiamondException("Bad Type Evaluation", right()))
            }
        }
      } else {
        val lhs = left().eval(env)
        val rhs = right().eval(env)

        if (lhs.isLeft || rhs.isLeft) {
          Left(new DiamondException("Failed to evaluate", this))
        }

        computeOperator(lhs.right.get, operator(), rhs.right.get) match {
          case Left(e)      => Left(new DiamondException(e.getMessage, this))
          case Right(value) => Right(Option(value))
        }
      }
    }

    private def computeAssign(env: Environment,
                              rhs: Any): Either[Throwable, Any] = {
      left() match {
        case name: Name =>
          Right(env.put(name.name(), rhs))
        case _ =>
          Left(new DiamondException("Bad Type", this))
      }
    }

    private def computeOperator(lhs: Option[Any],
                                operator: String,
                                rhs: Option[Any]): Either[Throwable, Any] = {
      if (lhs.get.isInstanceOf[Int] && rhs.get.isInstanceOf[Int]) {
        computeNumber(lhs.get.asInstanceOf[Int],
                      operator,
                      rhs.get.asInstanceOf[Int]) // TODO
      } else {
        operator match {
          case PLUS =>
            Right(
              new StringBuilder()
                .append(lhs.getOrElse(EMPTY))
                .append(rhs.getOrElse(EMPTY))
                .toString())
          case EQ =>
            lhs match {
              case Some(_) =>
                rhs match {
                  case Some(_) => Right(FALSE)
                  case None    => Right(TRUE)
                }
              case None => if (lhs == rhs) Right(TRUE) else Right(FALSE)
            }
          case _ => Left(new DiamondException("Operation is bad", this))
        }
      }
    }

    private def computeNumber(lhs: Int,
                              operator: String,
                              rhs: Int): Either[Throwable, Any] = {
      if (operator == PLUS) {
        Right(lhs + rhs)
      } else if (operator == MINUS) {
        Right(lhs - rhs)
      } else if (operator == MUL) {
        Right(lhs * rhs)
      } else if (operator == DIV) {
        Right(lhs / rhs)
      } else if (operator == RES) {
        Right(lhs % rhs)
      } else if (operator == EQ) {
        Right(lhs == rhs)
      } else if (operator == GT) {
        Right(lhs > rhs)
      } else if (operator == LT) {
        Right(lhs < rhs)
      } else {
        Left(new DiamondException("Operator is not supported", this))
      }
    }
  }

  // PrimaryExpr

  object PrimaryExpr {

    def newInstance(c: List[ASTree]): PrimaryExpr = new PrimaryExpr(c.asJava)

    def create(c: List[ASTree]): ASTree = {
      c.size match {
        case 1 => c.head
        case _ => new PrimaryExpr(c.asJava)
      }
    }
  }

  case class PrimaryExpr(_c: java.util.List[ASTree]) extends ASTList(_c) {

    override def eval(env: Environment): Either[DiamondException, Option[Any]] =
      Right(Some())
  }

  // NegativeExpr

  case class NegativeExpr(_c: java.util.List[ASTree]) extends ASTList(_c) {

    def operand(): ASTree = child(0).get

    override def eval(env: Environment): Either[DiamondException, Option[Any]] =
      Left(new DiamondException("Not implemented", this))

    override def toString(): String = "-" + operand()
  }

  // BlockStatement

  case class BlockStatement(_c: java.util.List[ASTree]) extends ASTList(_c) {

    override def eval(
        env: Environment): Either[DiamondException, Option[Any]] = {
      // TODO correct?
      this.toList
        .filterNot(t => t.isInstanceOf[NullStatement])
        .map(t => t.eval(env))
        .last
    }
  }

  // IfStatement

  case class IfStatement(_c: java.util.List[ASTree]) extends ASTList(_c) {

    def condition(): ASTree = child(0).get

    def thenBlock(): ASTree = child(1).get

    def elseBlock(): Option[ASTree] =
      if (numberOfChildren() > 2) Some(child(2).get) else None

    override def eval(
        env: Environment): Either[DiamondException, Option[Any]] = {
      condition().eval(env) match {
        case Left(e) => Left(e)
        case Right(c) => {
          c match {
            case Some(cv) =>
              if (cv.isInstanceOf[Int] && cv.asInstanceOf[Int] != FALSE) {
                thenBlock().eval(env)
              } else {
                elseBlock() match {
                  case Some(block) => block.eval(env)
                  case None        => Right(Some(0))
                }
              }
          }
        }
      }
    }

    override def toString(): String =
      "(if " + condition() + " " + thenBlock() + " else " + elseBlock()
        .getOrElse(EMPTY) + ")"
  }

  // WhileStatement

  case class WhileStatement(_c: java.util.List[ASTree]) extends ASTList(_c) {

    def condition(): ASTree = child(0).get

    def body(): ASTree = child(1).get

    override def eval(
        env: Environment): Either[DiamondException, Option[Any]] = {
      var result: Either[DiamondException, Option[Any]] = Right(None)
      while (true) {
        val c = condition().eval(env)
        if (c.right.get
              .isInstanceOf[Int] && c.right.get.asInstanceOf[Int] == FALSE) {
          return Right(Some(result))
        } else {
          result = body.eval(env)
        }
      }
      // never reacheable??
      result
    }

    override def toString(): String =
      "(while " + condition() + " " + body() + ")"
  }

  // NullStatement

  case class NullStatement(_c: java.util.List[ASTree]) extends ASTList(_c) {

    override def eval(env: Environment): Either[DiamondException, Option[Any]] =
      Left(new DiamondException("Not implemented", this))
  }
}

sealed class ASTList(_list: java.util.List[ASTree]) extends ASTree {

  protected val children: List[ASTree] = _list.asScala.toList

  override def child(i: Int): Option[ASTree] = Option(children.apply(i))

  override def numberOfChildren(): Int = children.size

  override def into_iter(): Iterator[ASTree] = children.iterator

  override def location: String = {
    children.map(f => f.location).filter(s => !s.isEmpty).head
  }

  override def eval(env: Environment): Either[DiamondException, Option[Any]] = {
    Left(new DiamondException("Bad Operation", this))
  }

  override def toString(): String = {
    val joiner = new StringJoiner(" ")
    children.map(e => e.toString()).foreach(_ => joiner.add(_))
    joiner.toString
  }
}
