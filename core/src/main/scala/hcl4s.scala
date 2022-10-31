package dev.jtrim777

import hcl4s.eval.HCLFunction
import hcl4s.model.{HCLBody, HCLValue}
import hcl4s.util.HCLError

import java.nio.file.Path

package object hcl4s {
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
