package dev.jtrim777.hcl4s

import eval.HCLFunction
import model.{HCLBody, HCLValue}
import util.HCLError

import java.nio.file.Path

object core {
  def fromFile(path: Path, env: Map[String, HCLValue] = Map.empty,
               functions: Map[String, HCLFunction] = Map.empty): Either[HCLError, HCLBody] = for {
    source <- parser.attemptLoad(path)
    body <- eval.attemptEval(source, env, functions)
  } yield body

  def fromString(text: String, env: Map[String, HCLValue] = Map.empty,
               functions: Map[String, HCLFunction] = Map.empty): Either[HCLError, HCLBody] = for {
    source <- parser.attemptParse(text)
    body <- eval.attemptEval(source, env, functions)
  } yield body
}
