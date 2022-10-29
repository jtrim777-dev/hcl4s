package dev.jtrim777.hcl4s.parser

import dev.jtrim777.hcl4s.lang.expr.{Expression, ValueType}
import dev.jtrim777.hcl4s.lang.expr.operators.{BinaryOperator, UnaryOperator}
import dev.jtrim777.hcl4s.lang.expr.operators

object helpers {
  def parseNumber(txt: String): Expression.Literal = {
    if (txt.contains(".")) {
      Expression.Literal(ValueType.FloatingValue(txt.toDouble))
    } else if (txt.startsWith("0x")) {
      Expression.Literal(ValueType.IntegerValue(java.lang.Long.valueOf(txt.drop(2), 16)))
    } else if (txt.startsWith("0b")) {
      Expression.Literal(ValueType.IntegerValue(java.lang.Long.valueOf(txt.drop(2), 2)))
    } else {
      Expression.Literal(ValueType.IntegerValue(txt.toLong))
    }
  }

  def parseBool(txt: String): Expression.Literal = txt match {
    case "true" => Expression.Literal(ValueType.BooleanValue(true))
    case "false" => Expression.Literal(ValueType.BooleanValue(false))
  }

  def parseUnOperator(raw: String): UnaryOperator = raw match {
    case "-" => operators.Negate
    case "!" => operators.Not
  }

  def parseBinOperator(raw: String): BinaryOperator[_, _] = {
    operators.Binaries.find(_.id == raw).get
  }
}
