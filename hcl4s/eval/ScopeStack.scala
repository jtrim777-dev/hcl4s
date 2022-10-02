package dev.jtrim777.hcl4s.eval

import dev.jtrim777.hcl4s.lang.expr.Expression.AbsoluteTerm

case class ScopeStack(head: Scope, tail: Option[ScopeStack]) {
  def push(): ScopeStack = ScopeStack(Scope.empty, Some(this))

  def pop(): ScopeStack = tail.getOrElse(throw new IllegalStateException("Cannot pop top-level scope"))

  def lookup(symbol: String): Option[AbsoluteTerm] = {
    head.lookup(symbol).orElse(tail.flatMap(_.lookup(symbol)))
  }

  def get(symbol: String): AbsoluteTerm = lookup(symbol).get

  def enscope(key: String, value: AbsoluteTerm): ScopeStack = this.copy(head = head.enscope(key, value))
}

object ScopeStack {
  def apply(vars: Map[String, AbsoluteTerm]): ScopeStack = ScopeStack(Scope(vars), None)

  def empty: ScopeStack = ScopeStack(Scope.empty, None)
}
