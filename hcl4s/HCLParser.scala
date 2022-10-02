package dev.jtrim777.hcl4s

import dev.jtrim777.hcl4s.eval.HCLEval
import dev.jtrim777.hcl4s.lang.expr.{Expression, ValueType, operators}
import dev.jtrim777.hcl4s.lang.struct.BodyElemT._
import lang.struct._

object HCLParser extends App {
  val test: HCLSource = HCLSourceT(List(
    AttributeT("rando", Expression.Literal(ValueType.IntegerValue(72))),
    BlockT("target", List("corretto"), List(
      AttributeT("blerb", Expression.UnaryOp(operators.Negate, Expression.Variable("rando")))
    ))
  ))

  pprint.pprintln(test)

  val result = HCLEval.evaluate(test, Map.empty)

  pprint.pprintln(result)
}
