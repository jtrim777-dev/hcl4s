package dev.jtrim777.hcl4s

import dev.jtrim777.hcl4s.lang.struct.BodyElemT.BlockT
import dev.jtrim777.hcl4s.lang.struct.{Block, HCLSource}
import dev.jtrim777.hcl4s.model.{HCLBody, HCLValue}
import dev.jtrim777.hcl4s.util.HCLError.EvaluationException

import scala.util.{Failure, Success, Try}

package object eval {
  type BlockNameStrategy = BlockT[_] => Option[String]

  def evaluate(source: HCLSource, env: Map[String, HCLValue] = Map.empty,
               functions: Map[String, HCLFunction] = Map.empty,
               blockNaming: Option[BlockNameStrategy] = Some(Context.DefaultBNStrat)): HCLBody = {
    HCLEval.evaluate(source, env, functions, scopeBlocks = blockNaming.isDefined,
      blockNameStrategy = blockNaming.getOrElse(Context.DefaultBNStrat))
  }

  def attemptEval(source: HCLSource, env: Map[String, HCLValue] = Map.empty,
                  functions: Map[String, HCLFunction] = Map.empty,
                  blockNaming: Option[BlockNameStrategy] = Some(Context.DefaultBNStrat)): Either[EvaluationException, HCLBody] = {
    Try(HCLEval.evaluate(source, env, functions, scopeBlocks = blockNaming.isDefined,
      blockNameStrategy = blockNaming.getOrElse(Context.DefaultBNStrat))) match {
      case Failure(ex: EvaluationException) => Left(ex)
      case Failure(other) => throw other
      case Success(value) => Right(value)
    }
  }
}
