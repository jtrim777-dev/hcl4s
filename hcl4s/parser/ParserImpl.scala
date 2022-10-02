package dev.jtrim777.hcl4s.parser

import org.parboiled2._

import collection.immutable.Seq
import dev.jtrim777.hcl4s.lang.expr.{ValueType, Expression => expr}

class ParserImpl(val input: ParserInput) extends Parser {
  implicit class StringRuleGen(value:String) {
    def ws: Rule0 = rule {
      str(value) ~ oneOrMore(' ')
    }

    def capKey: Rule1[String] = rule {
      capture(atomic(str(value))) ~ !(CharPredicate.AlphaNum | '_') ~ WSLOp
    }

    def sym: Rule0 = rule {
      atomic(str(value)) ~ zeroOrMore(' ')
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

  def WSOp: Rule0 = rule { zeroOrMore(' ') }
  def WSReq: Rule0 = rule { oneOrMore(' ') }
  def WSLOp: Rule0 = rule { zeroOrMore(anyOf(" \n")) }
  def WSLReq: Rule0 = rule { oneOrMore(anyOf(" \n")) }

  def LP: Rule0 = rule { "(".wsl }
  def RP: Rule0 = rule { ")".wsl }
  def LB: Rule0 = rule { "[".wsl }
  def RB: Rule0 = rule { "]".wsl }
  def LC: Rule0 = rule { "{".wsl }
  def RC: Rule0 = rule { "}".wsl }

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

  def DecLiteral: Rule1[String] = rule {
    capture(("-".? ~ CharPredicate.Digit19 ~ zeroOrMore(CharPredicate.Digit)) | '0')
  }
  def HexLiteral: Rule1[String] = rule {
    capture("-".? ~ "0x" ~ oneOrMore(CharPredicate.HexDigit))
  }
  def BinLiteral: Rule1[String] = rule {
    capture("-".? ~ "0b" ~ oneOrMore(ch('0') | ch('1')))
  }

  def IntLiteral: Rule1[expr] = rule {
    ((HexLiteral | BinLiteral | DecLiteral) ~> helpers.parseNumber _) ~ WSLOp
  }
  // TODO: Floating point support

  def BoolLit: Rule1[expr] = rule {
    capture("true" | "false") ~ !(CharPredicate.AlphaNum | '_') ~> helpers.parseBool _
  }

  def NullLit: Rule1[expr] = rule {
    capture("null") ~ !(CharPredicate.AlphaNum | '_') ~> {_:String => expr.Literal(ValueType.NullValue)}
  }
}
