package dev.jtrim777.hcl4s

import eval.{BlockNameStrategy, Context, HCLFunction}
import model.{HCLBody, HCLValue}
import util.HCLError

import java.nio.file.Path
import dev.jtrim777.hcl4s.{functions => fx}

object HCL {
  def fromFile(path: Path, env: Map[String, HCLValue] = Map.empty,
               functions: Map[String, HCLFunction] = Map.empty,
               blockNaming: Option[BlockNameStrategy] = Some(Context.DefaultBNStrat)): Either[HCLError, HCLBody] = for {
    source <- parser.attemptLoad(path)
    body <- eval.attemptEval(source, env, functions ++ fx.BaseFunctions, blockNaming = blockNaming)
  } yield body

  def fromString(text: String, env: Map[String, HCLValue] = Map.empty,
                 functions: Map[String, HCLFunction] = Map.empty,
                 blockNaming: Option[BlockNameStrategy] = Some(Context.DefaultBNStrat)): Either[HCLError, HCLBody] = for {
    source <- parser.attemptParse(text)
    body <- eval.attemptEval(source, env, functions ++ fx.BaseFunctions, blockNaming = blockNaming)
  } yield body
}
