package dev.jtrim777.hcl4s.parser

import org.parboiled2._

trait IDHelpers extends Parser with WSHelpers {
  def CoreID: Rule0 = rule {
    ("_" | CharPredicate.Alpha) ~ zeroOrMore(CharPredicate.AlphaNum | ch('_') | ch('.') | ch('$'))
  }

  def CappedID: Rule1[String] = rule {
    capture(CoreID) ~ !(CharPredicate.AlphaNum | '_')
  }

  def ID: Rule1[String] = rule {
    CappedID ~ WSLOp
  }

  def IDNoWS: Rule1[String] = CappedID

  def IDNoRet: Rule1[String] = rule {
    CappedID ~ WSOp
  }
}
