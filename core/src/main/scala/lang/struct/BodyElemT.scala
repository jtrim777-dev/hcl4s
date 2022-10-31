package dev.jtrim777.hcl4s.lang.struct

import dev.jtrim777.hcl4s.lang.expr.Expression
import dev.jtrim777.hcl4s.util.Traceable

sealed trait BodyElemT[T <: Expression] extends Traceable {

}

object BodyElemT {
  case class AttributeT[T <: Expression](name: String, value: T) extends BodyElemT[T] {
    override def traceType: String = "attribute"
    override def traceDisplay: String = s"$name = ${value.traceDisplay}"
    override def shortDisplay: String = s"$name = ${value.shortDisplay}"
  }

  case class BlockT[T <: Expression](kind: String, labels: List[String], content: List[BodyElemT[T]]) extends BodyElemT[T] {
    override def traceType: String = "block"

    override def traceDisplay: String = {
      val ltext = labels.map(s => "\"" + s + "\"").mkString(" ", " ", " ")

      s"$kind$ltext{...}"
    }

    override def shortDisplay: String = traceDisplay

    def key: String = kind + (if (labels.isEmpty) "" else labels.mkString(".", ".", ""))
  }
}
