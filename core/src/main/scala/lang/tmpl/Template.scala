package dev.jtrim777.hcl4s.lang.tmpl

import dev.jtrim777.hcl4s.util.Traceable

case class Template(content: List[TemplateItem]) extends Traceable {
  override val traceType: String = "template"
  override def traceDisplay: String = "\"" + content.map(_.traceDisplay).mkString("") + "\""
  override def shortDisplay: String = "\"" + content.headOption.map(_.shortDisplay + "...").getOrElse("") + "\""
}

