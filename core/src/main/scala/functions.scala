package dev.jtrim777.hcl4s

import eval.{Context, HCLFunction}
import lang.expr.Expression
import lang.expr.Expression.AbsoluteTerm
import parser.attemptParseTemplate

import java.nio.file.{Files, Paths}
import scala.util.{Failure, Success, Try}

object functions {
  val LoadTemplate: HCLFunction = HCLFunction("template", { (args: List[AbsoluteTerm], ctx: Context) =>
    if (args.length != 1) {
      ctx.throwError("Function `template` expects 1 string argument 'filepath'")
    } else {
      args.head match {
        case Expression.ResolvedTmpl(value) =>
          val path = Paths.get(value)
          if (!Files.exists(path)) {
            ctx.throwError("No such file \"" + path.toAbsolutePath.toString + "\"")
          } else {
            Try(Files.readString(path)) match {
              case Failure(exception) => ctx.throwError("Could not read template file at path \""+
                path.toAbsolutePath.toString + "\": " + exception.toString)
              case Success(fileContent) => attemptParseTemplate(fileContent) match {
                case Left(err) => ctx.throwError("Could not parse template file at path \"" +
                  path.toAbsolutePath.toString + "\": " + err.toString)
                case Right(value) => ctx.evaluate(value)
              }
            }
          }
        case _ => ctx.throwError("Function `template` expects 1 string argument 'filepath'")
      }
    }
  })
}
