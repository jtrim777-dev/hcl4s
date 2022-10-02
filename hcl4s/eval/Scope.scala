package dev.jtrim777.hcl4s.eval

import dev.jtrim777.hcl4s.lang.expr.Expression.AbsoluteTerm

case class Scope(variables: Map[String, AbsoluteTerm]) {
  def lookup(symbol: String): Option[AbsoluteTerm] = variables.get(symbol)
  def get(symbol: String): AbsoluteTerm = variables(symbol)
  def enscope(key: String, value: AbsoluteTerm): Scope = this.copy(variables = variables.updated(key, value))
}

object Scope {
  def empty: Scope = Scope(Map.empty)
}
