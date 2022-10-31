package dev.jtrim777
package hcl4s.parser

import hcl4s.lang.expr.{Expression => expr}
import hcl4s.lang.tmpl.{Template => tmpl, TemplateItem => tmpli}

import fastparse._, NoWhitespace._
import FastparseUtils._
import IDHelpers.ID

private[parser] object TemplateParser {
  def Expression[_: P]: Rule1[expr] = Parser.Expression

  def space[_: P]: Rule0 = P( CharsWhileIn(" \r\n\t", 0) )
  def needSpace[_: P]: Rule0 = P( CharsWhileIn(" \r\n\t", 1) )

  def escape[_: P](id: String): Rule1[Boolean] = P {
    !(id ~~ id) ~ id ~~ "{" ~ "~".!.? ~ space ~> {(o:Option[String]) => o.isDefined}
  }

  def endEscape[_: P]: Rule1[Boolean] = P {
    space ~ "~".!.? ~ "}" ~> {(o:Option[String]) => o.isDefined}
  }

  def Interpolation[_: P]: Rule1[tmpli.Interpolation] = P {
    (escape("$") ~/ Expression ~ endEscape) ~> {data => tmpli.Interpolation(data._1, data._2, data._3)}
  }

  def IfElse[_: P]: Rule1[tmpl] = P {
    escape("%") ~/ "else" ~ endEscape ~ Template ~> {t => t._3}
  }
  def IfDirective[_: P]: Rule1[tmpli.TmplIf] = P {
    escape("%") ~/ "if" ~/ needSpace ~ Expression ~ endEscape ~
      Template ~ IfElse.? ~
      escape("%") ~/ "endif" ~/ endEscape ~>
      {data => tmpli.TmplIf(data._1, data._2, data._4, data._5, data._7)}
  } // TODO: Make stripping work properly

  def ForDirective[_: P]: Rule1[tmpli.TmplFor] = P {
    escape("%") ~/ "for" ~ needSpace ~/ ID ~ ("," ~ space ~/ ID ~ needSpace).? ~ "in" ~ needSpace ~ Expression ~ endEscape ~
      Template ~
      escape("%") ~/ "endfor" ~ endEscape ~>
      {data => tmpli.TmplFor(data._1, data._2, data._3, data._4, data._6, data._8)}
  }

  def Directive[_: P]: Rule1[tmpli.Directive] = P {
    ForDirective | IfDirective
  }

  def BaseChar[_: P]: Rule1[String] = P {
    P("$${").push("${") | P("%%{").push("%{") | (!"%{" ~ !"${" ~ AnyChar.!)
  }
  def RawLiteral[_: P]: Rule1[String] = P {
    BaseChar.rep(1) ~> {(chars:Seq[String]) => chars.mkString("")}
  }
  def Literal[_: P]: Rule1[tmpli.Literal] = P {
    RawLiteral ~> tmpli.Literal.apply
  }

  def TemplateItem[_: P]: Rule1[tmpli] = P {
    Interpolation | Directive | Literal
  }

  def Template[_: P]: Rule1[tmpl] = P {
    TemplateItem.repX(1).? ~ End ~> {
      case Some(items) => tmpl(items.toList)
      case None => tmpl(List.empty)
    }
  }
}

