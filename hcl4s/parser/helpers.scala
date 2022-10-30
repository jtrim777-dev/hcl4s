package dev.jtrim777.hcl4s.parser

import dev.jtrim777.hcl4s.lang.expr.Expression.Term
import dev.jtrim777.hcl4s.lang.expr.{Expression, ValueType}
import dev.jtrim777.hcl4s.lang.expr.operators.{BinaryOperator, PostfixOp, UnaryOperator}
import dev.jtrim777.hcl4s.lang.expr.operators

private[parser] object helpers {
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

  def combineTerm(base: Term, ops: Seq[PostfixOp]): Term = {
    if (ops.isEmpty) base else {
      val next = ops.head match {
        case operators.AccessOp(key) => base match {
          case Expression.AttrSplat(tgt, exts) => Expression.AttrSplat(tgt, exts :+ key)
          case Expression.FullSplat(tgt, exts) => Expression.FullSplat(tgt, exts :+ Left(key))
          case _ => Expression.GetAttr(base, key)
        }
        case operators.IndexOp(value) => base match {
          case Expression.FullSplat(tgt, exts) => Expression.FullSplat(tgt, exts :+ Right(value))
          case _ => Expression.Index(base, value)
        }
        case operators.ASplatOp => Expression.AttrSplat(base, List.empty)
        case operators.FSplatOp => Expression.FullSplat(base, List.empty)
      }

      combineTerm(next, ops.tail)
    }
  }
}
