package dev.jtrim777.hcl4s.eval

import dev.jtrim777.hcl4s.lang.expr.Expression.AbsoluteTerm

private[eval] case class HCLFunction(name: String, operation: List[AbsoluteTerm] => AbsoluteTerm) {
  def apply(args: List[AbsoluteTerm]): AbsoluteTerm = operation(args)
}
