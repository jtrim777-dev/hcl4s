package dev.jtrim777.hcl4s

import dev.jtrim777.hcl4s.lang.struct.HCLSource
import dev.jtrim777.hcl4s.model.{HCLBody, HCLValue}

import scala.util.{Failure, Success, Try}

package object eval {
  def evaluate(source: HCLSource, env: Map[String, HCLValue] = Map.empty,
               functions: Map[String, HCLFunction] = Map.empty): HCLBody = {
    HCLEval.evaluate(source, env, functions)
  }

  def attemptEval(source: HCLSource, env: Map[String, HCLValue] = Map.empty,
               functions: Map[String, HCLFunction] = Map.empty): Either[HCLEvaluationException, HCLBody] = {
    Try(HCLEval.evaluate(source, env, functions)) match {
      case Failure(ex:HCLEvaluationException) => Left(ex)
      case Failure(other) => throw other
      case Success(value) => Right(value)
    }
  }

  case class HCLEvaluationException(message: String, trace: Trace) extends Exception {
    override def getLocalizedMessage: String = message + "\n" + trace.format.mkString("\n")
  }
}
