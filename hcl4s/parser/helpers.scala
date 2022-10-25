package dev.jtrim777.hcl4s.parser

import dev.jtrim777.hcl4s.lang.expr.Expression
import dev.jtrim777.hcl4s.lang.expr.operators.{BinaryOperator, UnaryOperator}

object helpers {
  def parseNumber(txt: String): Expression.Literal = ???
  def parseBool(txt: String): Expression.Literal = ???

  def parseUnOperator(raw: String): UnaryOperator = ???
  def parseBinOperator(raw: String): BinaryOperator[_, _] = ???
}
