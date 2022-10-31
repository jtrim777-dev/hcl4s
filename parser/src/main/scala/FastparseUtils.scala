package dev.jtrim777.hcl4s.parser

import fastparse._

private[parser] object FastparseUtils {
  type Rule1[+T] = P[T]
  type Rule0 = P[Unit]

  implicit class Rule1Ops[_:P, +T](val rule: Rule1[T]) {
    def ~>[R](fx: T => R): Rule1[R] = P {
      rule.map(fx)
    }

//    def +(implicit whitespace: P[_] => P[Unit]) : Rule1[Seq[T]] = P { rule.rep(1) }
//    def +(sep: Rule0)(implicit whitespace: P[_] => P[Unit]) : Rule1[Seq[T]] = P { rule.rep(1, sep=sep) }
//
//    def *(implicit whitespace: P[_] => P[Unit]): Rule1[Seq[T]] = P { rule.rep(0) }
//    def *(sep: Rule0)(implicit whitespace: P[_] => P[Unit]): Rule1[Seq[T]] = P { rule.rep(0, sep = sep) }
  }

  implicit class Rule0Ops[_: P](val rule: Rule0) {
    def ~>[R](fx: => R): Rule1[R] = P { rule.map{_ => fx} }

    def push[R](value: => R): Rule1[R] = P { rule.map(_ => value) }
  }

  def CharDigit[_: P]: Rule0 = P( CharIn("0-9") )
  def CharDigit19[_: P]: Rule0 = P( CharIn("1-9") )
  def CharHexDigit[_: P]: Rule0 = P( CharIn("0-9a-fA-F") )
  def CharAlpha[_: P]: Rule0 = P( CharIn("a-zA-Z") )
  def CharAlphaNum[_: P]: Rule0 = P( CharIn("0-9a-zA-Z") )
}
