package dev.jtrim777.hcl4s.parser

import org.parboiled2._
import dev.jtrim777.hcl4s.lang.tmpl.{TemplateItem => tmpli}
import dev.jtrim777.hcl4s.lang.tmpl.{Template => tmpl}
import dev.jtrim777.hcl4s.lang.expr.{Expression => expr}

class TemplateParser(val input: ParserInput) extends Parser with SymHelpers with IDHelpers with StringBuilding {
  def Expression: Rule1[expr] = rule {
    runSubParser(inp => (new ParserImpl(inp)).Expression)
  }

  def escape(id: Char): Rule1[Boolean] = rule {
    !(ch(id) ~ ch(id)) ~ ch(id) ~ '{' ~ capture('~').? ~ WSOp ~> {(o:Option[String]) => o.isDefined}
  }

  def endEscape: Rule1[Boolean] = rule {
    WSOp ~ capture('~').? ~ '}' ~> {(o:Option[String]) => o.isDefined}
  }

  def Interpolation: Rule1[tmpli.Interpolation] = rule {
    escape('$') ~ Expression ~ endEscape ~> {(sh:Boolean, exp: expr, st:Boolean) => tmpli.Interpolation(sh, exp, st)}
  }

  def IfElse: Rule1[tmpl] = rule {
    escape('%') ~ "else".keyword ~ endEscape ~ Template ~> {(_:Boolean, _:Boolean, a: tmpl) => a}
  }
  def IfDirective: Rule1[tmpli.TmplIf] = rule {
    escape('%') ~ "if".keyword ~ Expression ~ endEscape ~
      Template ~ IfElse.? ~
      escape('%') ~ WSOp ~ "endif".keyword ~ endEscape ~>
      {(sh1:Boolean, cond:expr, _: Boolean, tmp: tmpl, tmp2: Option[tmpl], _:Boolean, st2:Boolean) =>
        tmpli.TmplIf(sh1, cond, tmp, tmp2, st2)}
  } // TODO: Make stripping work properly

  def ForDirective: Rule1[tmpli.TmplFor] = rule {
    escape('%') ~ "for".keyword ~ ID ~ (",".sym ~ ID).? ~ "in".keyword ~ Expression ~ endEscape ~
      Template ~
      escape('%') ~ "endfor".keyword ~ endEscape ~>
      {(sh:Boolean, i1:String, i2:Option[String], seq: expr, _:Boolean, t:tmpl, _:Boolean, st:Boolean) =>
        tmpli.TmplFor(sh, i1, i2, seq, t, st)
      }
  }

  def Directive: Rule1[tmpli.Directive] = rule {
    ForDirective | IfDirective
  }

  def BaseChar: Rule1[String] = rule {
    !str("%{") ~ !str("${") ~ capture(ANY)
  }
  def RawLiteral: Rule1[String] = rule {
    zeroOrMore(BaseChar) ~> {(chars:Seq[String]) => chars.mkString("")}
  }
  def Literal: Rule1[tmpli.Literal] = rule {
    RawLiteral ~> tmpli.Literal.apply _
  }

  def TemplateItem: Rule1[tmpli] = rule {
    Interpolation | Directive | Literal
  }

  def Template: Rule1[tmpl] = rule {
    oneOrMore(TemplateItem) ~ EOI ~> {(items:Seq[tmpli]) => tmpl(items.toList)}
  }
}

