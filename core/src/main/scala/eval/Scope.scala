package dev.jtrim777.hcl4s.eval

import dev.jtrim777.hcl4s.lang.expr.Expression
import dev.jtrim777.hcl4s.lang.expr.Expression.AbsoluteTerm

private[eval] case class Scope(variables: Map[String, AbsoluteTerm]) {
  def lookup(symbol: String): Option[AbsoluteTerm] = variables.get(symbol)
  def get(symbol: String): AbsoluteTerm = variables(symbol)
  def enscope(key: String, value: AbsoluteTerm): Scope = this.copy(variables = variables.updated(key, value))
  def enscope(key: List[String], value: AbsoluteTerm): Scope = {
    if (key.isEmpty) {
      throw new IllegalArgumentException("Cannot enscope for empty key")
    } else if (key.length == 1) {
      this.enscope(key.head, value)
    } else {
      variables.get(key.head) match {
        case Some(map: Expression.AbsMapping) => this.copy(variables = variables.updated(key.head, map.add(key.tail, value)))
        case _ => this.copy(variables = variables.updated(key.head, Expression.AbsMapping(Map.empty).add(key.tail, value)))
      }
    }
  }
}

private[eval] object Scope {
  def empty: Scope = Scope(Map.empty)
}
