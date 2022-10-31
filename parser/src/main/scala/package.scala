package dev.jtrim777.hcl4s

import dev.jtrim777.hcl4s.lang.expr.Expression
import dev.jtrim777.hcl4s.lang.struct.HCLSource
import dev.jtrim777.hcl4s.util.HCLError

import java.nio.file.{Files, Path}
import fastparse.{Parsed, parse => libparse}

package object parser {
  def parse(source: String): HCLSource = {
    libparse(source, Parser.HCL(_)) match {
      case Parsed.Success(value, _) => value
      case failure: Parsed.Failure => throw new IllegalArgumentException(failure.longMsg)
    }
  }

  def load(path: Path): HCLSource = parse(Files.readString(path))

  def attemptParse(source: String): Either[HCLError.ParsingFailure, HCLSource] = {
    libparse(source, Parser.HCL(_)) match {
      case Parsed.Success(value, _) => Right(value)
      case failure: Parsed.Failure => Left(HCLError.ParsingFailure(failure.toString(), failure))
    }
  }

  def attemptLoad(path: Path): Either[HCLError.ParsingFailure, HCLSource] = attemptParse(Files.readString(path))

  def parseTemplate(source: String): Expression.TmplExpr = {
    libparse(source, TemplateParser.Template(_)) match {
      case Parsed.Success(value, _) => Expression.TmplExpr(value)
      case failure: Parsed.Failure => throw new IllegalArgumentException(failure.longMsg)
    }
  }

  def loadTemplate(path: Path): Expression.TmplExpr = parseTemplate(Files.readString(path))

  def attemptParseTemplate(source: String): Either[HCLError.ParsingFailure, Expression.TmplExpr] = {
    libparse(source, TemplateParser.Template(_)) match {
      case Parsed.Success(value, _) => Right(Expression.TmplExpr(value))
      case failure: Parsed.Failure => Left(HCLError.ParsingFailure(failure.toString(), failure))
    }
  }

  def attemptLoadTemplate(path: Path): Either[HCLError.ParsingFailure, Expression.TmplExpr] = attemptParseTemplate(Files.readString(path))
}
