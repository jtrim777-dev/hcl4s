package dev.jtrim777.hcl4s.util

case class Trace(head: Traceable, tail: Option[Trace]) {
  def push(item: Traceable): Trace = Trace(item, Some(this))

  def format: List[String] = if (tail.isEmpty) {
    List.empty
  } else {
    head.lineText :: tail.get.formatShort
  }

  def formatShort: List[String] = if (tail.isEmpty) {
    List.empty
  } else {
    head.lineText(true) :: tail.get.format
  }
}

object Trace {
  def empty: Trace = Trace(Root, None)

  object Root extends Traceable {
    override def traceType: String = "."

    override def traceDisplay: String = ""

    override def shortDisplay: String = ""
  }
}
