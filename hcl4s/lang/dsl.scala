package dev.jtrim777.hcl4s.lang

import dev.jtrim777.hcl4s.lang.expr.{Expression, ValueType}
import dev.jtrim777.hcl4s.lang.struct.BodyElemT.{AttributeT, BlockT}
import dev.jtrim777.hcl4s.lang.struct.{Attribute, Block, BodyElem, HCLSource, HCLSourceT}
import dev.jtrim777.hcl4s.lang.tmpl.{Template, TemplateItem}

import scala.language.implicitConversions

object dsl {
  implicit def strAsTemplate(str: String): Expression.Term = Expression.TmplExpr(Template(List(TemplateItem.Literal(str))))
  implicit def intAsLiteral(int: Int): Expression.Term = Expression.Literal(ValueType.IntegerValue(int))
  implicit def floatAsLiteral(float: Float): Expression.Term = Expression.Literal(ValueType.FloatingValue(float))
  implicit def longAsLiteral(lng: Long): Expression.Term = Expression.Literal(ValueType.IntegerValue(lng))
  implicit def doubleAsLiteral(dbl: Double): Expression.Term = Expression.Literal(ValueType.FloatingValue(dbl))
  implicit def boolAsLiteral(bool: Boolean): Expression.Term = Expression.Literal(ValueType.BooleanValue(bool))
  val Null: Expression.Literal = Expression.Literal(ValueType.NullValue)

  def hcl(elems: BodyElem*): HCLSource = HCLSourceT(elems.toList)
  def block(kind: String, tags: String*)(entries: BodyElem*): Block = BlockT(kind, tags.toList, entries.toList)
  def seq(items: Expression*): Expression.SequenceT = Expression.SequenceT(items.toList)
  def obj(items: (Expression, Expression)*): Expression.MappingT = {
    val map = Map(items:_*)
    Expression.MappingT(map)
  }

  def tmpl(tmpl: TemplateItem*): Expression = Expression.TmplExpr(Template(tmpl.toList))
  def interpolate(exp: Expression): TemplateItem = TemplateItem.Interpolation(stripStart = false, exp, stripEnd = false)

  implicit class StrExt(val str: String) {
    def :=(value: Expression): Attribute = AttributeT(str, value)

    def ~>(value: Expression): (Expression, Expression) = Expression.AttrKey(str) -> value

    def asVar: Expression.Variable = Expression.Variable(str)

    def asLit: TemplateItem.Literal = TemplateItem.Literal(str)

    def asKey: Expression.AttrKey = Expression.AttrKey(str)
  }

  implicit class TIExt(val ti: TemplateItem) {
    def ~(next: TemplateItem): Template = Template(List(ti, next))
  }

  implicit class TmplExt(val tmpl: Template) {
    def ~(next: TemplateItem): Template = Template(tmpl.content :+ next)
  }
}
