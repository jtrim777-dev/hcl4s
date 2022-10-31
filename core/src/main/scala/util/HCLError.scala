package dev.jtrim777.hcl4s.util

trait HCLError extends Exception {

}

object HCLError {
  case class ParsingFailure(message: String) extends HCLError {
    override def getMessage: String = message
  }

  case class EvaluationException(message: String, trace: Trace) extends HCLError {
    override def getLocalizedMessage: String = message + "\n" + trace.format.mkString("  ", "\n  ", "\n--------------")
  }
}