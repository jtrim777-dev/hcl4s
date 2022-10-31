package dev.jtrim777.hcl4s.eval

import dev.jtrim777.hcl4s.lang.expr.Expression
import dev.jtrim777.hcl4s.lang.expr.Expression.AbsoluteTerm
import dev.jtrim777.hcl4s.lang.struct.Block
import dev.jtrim777.hcl4s.lang.struct.BodyElemT.BlockT
import dev.jtrim777.hcl4s.util.HCLError.EvaluationException
import dev.jtrim777.hcl4s.util.{Trace, Traceable}

case class Context(scope: ScopeStack, trace: Trace, funcs: Map[String, HCLFunction], scopeBlocks: Boolean,
                   blockNameStrategy: BlockNameStrategy = Context.DefaultBNStrat) {
  def throwError(message: String): Nothing = throw EvaluationException(message, trace)

  def enscope(key: String, value: AbsoluteTerm): Context = this.copy(scope = scope.enscope(key, value))

  def enscope(key: List[String], value: AbsoluteTerm): Context = this.copy(scope = scope.enscope(key, value))

  def push(item: Traceable): Context = this.copy(scope = scope.push(), trace = trace.push(item))

  def evaluate(expr: Expression): AbsoluteTerm = ExprEval.evaluateExpression(expr, this)
}

object Context {
  val DefaultBNStrat: BlockT[_] => Option[String] = {b:BlockT[_] =>
    Some(b.kind + (if (b.labels.nonEmpty) b.labels.mkString(".", ".", "") else ""))
  }

  val NoKindBNStrat: BlockT[_] => Option[String] = { b: BlockT[_] =>
    if (b.labels.nonEmpty) Some(b.labels.mkString(".")) else None
  }
}
