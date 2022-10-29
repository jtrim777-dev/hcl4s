package dev.jtrim777
package hcl4s.parser

import hcl4s.lang.expr.{Expression => expr}
import hcl4s.lang.tmpl.{Template => tmpl, TemplateItem => tmpli}

import fastparse._, JavaWhitespace._
import FastparseUtils._
import IDHelpers2.ID

object TemplateParser2 {
  def Expression[_: P]: Rule1[expr] = Parser2.Expression

  def escape[_: P](id: String): Rule1[Boolean] = P {
    !(id ~~ id) ~ id ~~ "{" ~ "~".!.? ~> {(o:Option[String]) => o.isDefined}
  }

  def endEscape[_: P]: Rule1[Boolean] = P {
    "~".!.? ~ "}" ~> {(o:Option[String]) => o.isDefined}
  }

  def Interpolation[_: P]: Rule1[tmpli.Interpolation] = P {
    (escape("$") ~/ Expression ~ endEscape) ~> {data => tmpli.Interpolation(data._1, data._2, data._3)}
  }

  def IfElse[_: P]: Rule1[tmpl] = P {
    escape("%") ~/ "else" ~ endEscape ~ Template ~> {t => t._3}
  }
  def IfDirective[_: P]: Rule1[tmpli.TmplIf] = P {
    escape("%") ~/ "if" ~ Expression ~ endEscape ~
      Template ~ IfElse.? ~
      escape("%") ~/ "endif" ~ endEscape ~>
      {data => tmpli.TmplIf(data._1, data._2, data._4, data._5, data._7)}
  } // TODO: Make stripping work properly

  def ForDirective[_: P]: Rule1[tmpli.TmplFor] = P {
    escape("%") ~/ "for" ~ ID ~ ("," ~ ID).? ~ "in" ~ Expression ~ endEscape ~
      Template ~
      escape("%") ~/ "endfor" ~ endEscape ~>
      {data => tmpli.TmplFor(data._1, data._2, data._3, data._4, data._6, data._8)}
  }

  def Directive[_: P]: Rule1[tmpli.Directive] = P {
    ForDirective | IfDirective
  }

  def BaseChar[_: P]: Rule1[String] = P {
    !"%{" ~ !"${" ~ AnyChar.!
  }
  def RawLiteral[_: P]: Rule1[String] = P {
    BaseChar.repX ~> {(chars:Seq[String]) => chars.mkString("")}
  }
  def Literal[_: P]: Rule1[tmpli.Literal] = P {
    RawLiteral ~> tmpli.Literal.apply
  }

  def TemplateItem[_: P]: Rule1[tmpli] = P {
    Interpolation | Directive | Literal
  }

  def Template[_: P]: Rule1[tmpl] = P {
    TemplateItem.repX(1) ~ End ~> {(items:Seq[tmpli]) => tmpl(items.toList)}
  }
}

