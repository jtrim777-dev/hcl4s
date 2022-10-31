package dev.jtrim777.hcl4s

import dev.jtrim777.hcl4s.util.HCLError

import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

object BigTest extends App {
  def doIt(): Unit = {
    val basePath = Paths.get("/Users/jake/Documents/Projects/Libraries/hcl4s", "examples/bigtest")

    val variablesBody = HCL.fromFile(basePath.resolve("variables.hcl")) match {
      case Left(value) => throw value
      case Right(value) => value
    }
    val variables = variablesBody.toObject.data("variable")

    val mainBody = HCL.fromFile(basePath.resolve("main.hcl"), env = Map("var" -> variables), blockNaming = Some(eval.Context.NoKindBNStrat)) match {
      case Left(value) => throw value
      case Right(value) => value
    }

    val combinedBody = HCL.fromFile(basePath.resolve("combined.hcl")) match {
      case Left(value) => throw value
      case Right(value) => value
    }

    println(mainBody == combinedBody)
  }

  Try(doIt()) match {
    case Failure(err:HCLError) =>
      println(s"\u001b[31mProcessed failed with error of type ${err.getClass.getSimpleName}\u001b[0m")
      println(err.getMessage)
    case Failure(f) => throw f
    case Success(_) => ()
  }
}