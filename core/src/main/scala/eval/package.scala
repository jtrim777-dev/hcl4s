package dev.jtrim777.hcl4s

import dev.jtrim777.hcl4s.lang.struct.HCLSource
import dev.jtrim777.hcl4s.model.{HCLBody, HCLValue}
import dev.jtrim777.hcl4s.util.HCLError.EvaluationException

import scala.util.{Failure, Success, Try}

package object eval {
  def evaluate(source: HCLSource, env: Map[String, HCLValue] = Map.empty,
               functions: Map[String, HCLFunction] = Map.empty): HCLBody = {
    HCLEval.evaluate(source, env, functions)
  }

  def attemptEval(source: HCLSource, env: Map[String, HCLValue] = Map.empty,
               functions: Map[String, HCLFunction] = Map.empty): Either[EvaluationException, HCLBody] = {
    Try(HCLEval.evaluate(source, env, functions)) match {
      case Failure(ex:EvaluationException) => Left(ex)
      case Failure(other) => throw other
      case Success(value) => Right(value)
    }
  }
}
