package dev.jtrim777.hcl4s.eval

import dev.jtrim777.hcl4s.lang.expr.Expression.AbsoluteTerm

case class HCLFunction(name: String, operation: (List[AbsoluteTerm], Context) => AbsoluteTerm) {
  def apply(args: List[AbsoluteTerm], ctx: Context): AbsoluteTerm = operation(args, ctx)
}
