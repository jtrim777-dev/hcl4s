package dev.jtrim777.hcl4s.eval

import dev.jtrim777.hcl4s.lang.expr.Expression.AbsoluteTerm
import dev.jtrim777.hcl4s.util.Traceable

private[eval] case class Context(scope: ScopeStack, trace: Trace,
                                 funcs: Map[String, HCLFunction], scopeBlockTags: Boolean) {
  def throwError(message: String): Nothing = throw HCLEvaluationException(message, trace)

  def enscope(key: String, value: AbsoluteTerm): Context = this.copy(scope = scope.enscope(key, value))

  def enscope(key: List[String], value: AbsoluteTerm): Context = this.copy(scope = scope.enscope(key, value))

  def push(item: Traceable): Context = this.copy(scope = scope.push(), trace = trace.push(item))
}
