package dev.jtrim777.hcl4s.util

trait Traceable {
  def traceType: String
  def traceDisplay: String
  def shortDisplay: String

  def lineText(short: Boolean): String = {
    val txt = traceDisplay
    val body = if (txt.length > 30 || short) {
      shortDisplay
    } else txt

    val fmtBody = body.split('\n').map(s => "   " + s).mkString("\n")

    s"in $traceType$fmtBody"
  }

  def lineText: String = lineText(false)
}
