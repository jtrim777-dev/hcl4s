package dev.jtrim777.hcl4s.eval

import dev.jtrim777.hcl4s.lang.expr.Expression.AbsoluteTerm
import dev.jtrim777.hcl4s.lang.expr.operators._
import dev.jtrim777.hcl4s.lang.expr.{Expression, ValueType}

object operations {
  def evaluateOperation(op: BinaryOperator[_, _], lhs: AbsoluteTerm, rhs: AbsoluteTerm, ctx: Context): AbsoluteTerm = op match {
    case op: CompOp => comparison(op, lhs, rhs, ctx)
    case op: ArithOp => arithmetic(op, lhs, rhs, ctx)
    case op: LogicalOp => logical(op, lhs, rhs, ctx)
  }

  def arithmetic(op: ArithOp, lhs: AbsoluteTerm, rhs: AbsoluteTerm, ctx: Context): AbsoluteTerm = {
    val l = lhs.asInstanceOf[Expression.Literal].value.asInstanceOf[ValueType.NumericValue[_]].asDouble
    val r = rhs.asInstanceOf[Expression.Literal].value.asInstanceOf[ValueType.NumericValue[_]].asDouble

    Expression.Literal(ValueType.FloatingValue(op.doIt(l, r)))
  }

  def comparison(op: CompOp, lhs: AbsoluteTerm, rhs: AbsoluteTerm, ctx: Context): AbsoluteTerm = {
    val l = lhs.asInstanceOf[Expression.Literal].value.asInstanceOf[ValueType.NumericValue[_]].asDouble
    val r = rhs.asInstanceOf[Expression.Literal].value.asInstanceOf[ValueType.NumericValue[_]].asDouble

    Expression.Literal(ValueType.BooleanValue(op.doIt(l, r)))
  }

  def logical(op: LogicalOp, lhs: AbsoluteTerm, rhs: AbsoluteTerm, ctx: Context): AbsoluteTerm = {
    val l = lhs.asInstanceOf[Expression.Literal].value.asInstanceOf[ValueType.BooleanValue].value
    val r = rhs.asInstanceOf[Expression.Literal].value.asInstanceOf[ValueType.BooleanValue].value

    Expression.Literal(ValueType.BooleanValue(op.doIt(l, r)))
  }
}
