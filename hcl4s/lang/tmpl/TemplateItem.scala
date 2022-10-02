package dev.jtrim777.hcl4s.lang.tmpl

import dev.jtrim777.hcl4s.lang.expr.Expression

import dev.jtrim777.hcl4s.util.Traceable

sealed trait TemplateItem extends Traceable {
  val stripStart: Boolean
  val stripEnd: Boolean
}

object TemplateItem {
  case class Literal(value: String) extends TemplateItem {
    override val stripStart: Boolean = false
    override val stripEnd: Boolean = false

    override val traceType: String = "text literal"
    override def traceDisplay: String = value
    override def shortDisplay: String = "..."
  }

  case class Interpolation(stripStart: Boolean, value: Expression, stripEnd: Boolean) extends TemplateItem {
    override val traceType: String = "interpolation"
    override def traceDisplay: String = s"$${${value.traceDisplay}}"
    override def shortDisplay: String = "$${...}"
  }

  sealed trait Directive extends TemplateItem {
    val stripStart: Boolean
    val stripEnd: Boolean
  }
  case class TmplIf(stripStart: Boolean, cond: Expression,
                    resolve: Template, alt: Option[Template], stripEnd: Boolean) extends Directive {
    override val traceType: String = "if directive"
    override def traceDisplay: String = s"%{ if ${cond.traceDisplay} } ${resolve.traceDisplay} " +
      s"${alt.map(t => s"%{ else } ${t.traceDisplay} ").getOrElse("")} %{ endif }"
    override def shortDisplay: String = s"%{ if ... } ... ${alt.map(t => s"%{ else } ... ").getOrElse("")} %{ endif }"
  }


  case class TmplFor(stripStart: Boolean, primID: String, secID: Option[String], seqExpr: Expression,
                     resolve: Template, stripEnd: Boolean) extends Directive {
    override val traceType: String = "for directive"
    override def traceDisplay: String = s"%{ for $primID${secID.map(s => ", " + s).getOrElse("")} in ${seqExpr.traceDisplay} } ${resolve.traceDisplay} %{ endfor }"
    override def shortDisplay: String = "%{ for $primID${secID.map(s => \", \" + s).getOrElse(\"\")} in ... } ... %{ endfor }"
  }
}
