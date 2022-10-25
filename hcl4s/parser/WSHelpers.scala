package dev.jtrim777
package hcl4s.parser

import org.parboiled2._

trait WSHelpers extends Parser {
  implicit class StringRuleGen(value: String) {
    def ws: Rule0 = rule {
      str(value) ~ oneOrMore(' ')
    }

    def capKey: Rule1[String] = rule {
      capture(atomic(str(value))) ~ !(CharPredicate.AlphaNum | '_') ~ WSLOp
    }

    def sym: Rule0 = rule {
      atomic(str(value)) ~ WSOp
    }

    def symb: Rule0 = rule {
      atomic(str(value)) ~ WSLOp
    }

    def wsl: Rule0 = rule {
      str(value) ~ zeroOrMore(anyOf(" \n"))
    }

    def atom: Rule0 = rule {
      atomic(str(value))
    }

    def atomWS: Rule0 = rule {
      atomic(str(value)) ~ WSOp
    }

    def keyword: Rule0 = rule {
      atomic(str(value)) ~ WSReq
    }

    def keywordL: Rule0 = rule {
      atomic(str(value)) ~ WSLReq
    }
  }

  def WSOp: Rule0 = rule {
    zeroOrMore(' ')
  }

  def WSReq: Rule0 = rule {
    oneOrMore(' ')
  }

  def WSLOp: Rule0 = rule {
    zeroOrMore(anyOf(" \n"))
  }

  def WSLReq: Rule0 = rule {
    oneOrMore(anyOf(" \n"))
  }

  def NLReq: Rule0 = rule {
    zeroOrMore(' ') ~ '\n' ~ zeroOrMore(anyOf(" \n"))
  }
}
