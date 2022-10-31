package dev.jtrim777.hcl4s

import eval.{Context, HCLFunction}
import lang.expr.{Expression, ValueType}
import lang.expr.Expression.AbsoluteTerm
import parser.attemptParseTemplate

import java.nio.file.{Files, Paths}
import scala.util.{Failure, Success, Try}

object functions {
  val LoadTemplate: HCLFunction = HCLFunction("templatefile", { (args: List[AbsoluteTerm], ctx: Context) =>
    if (args.isEmpty || args.length > 2) {
      ctx.throwError("Function `templatefile` expects 1 string argument 'filepath' and optionally a map of variables")
    } else {
      args.head match {
        case Expression.ResolvedTmpl(value) =>
          val ictx = if (args.length > 1) {
            args(1) match {
              case Expression.AbsMapping(data) =>
                data.foldLeft(ctx) { case (c, (k, v)) => c.enscope(k, v)}
              case _ => ctx.throwError("Second argument to function `templatefile`, if provided, must be a Map")
            }
          } else ctx

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
                case Right(value) => ictx.evaluate(value)
              }
            }
          }
        case _ => ctx.throwError("Function `template` expects 1 string argument 'filepath'")
      }
    }
  })

  val GenerateRange: HCLFunction = HCLFunction("range", { (args: List[AbsoluteTerm], ctx: Context) =>
    if (args.length != 1) {
      ctx.throwError("Function `range` expects exactly one integer argument 'size'")
    } else {
      val size = args.head match {
        case Expression.Literal(ValueType.IntegerValue(i)) => i
        case _ => ctx.throwError("Function `range` expects exactly one integer argument 'size'")
      }

      if (size < 0) {
        ctx.throwError("'size' argument to function `range` must be at least 0")
      } else {
        Expression.AbsSequence((0 until size.toInt).toList.map(l => Expression.Literal(ValueType.IntegerValue(l))))
      }
    }
  })

  val BaseFunctions: Map[String, HCLFunction] = Map(
    LoadTemplate.name -> LoadTemplate,
    GenerateRange.name -> GenerateRange
  )
}
