package dev.jtrim777.hcl4s

import util.HCLError

import java.nio.file.{Paths, Files}
import scala.util.{Failure, Success, Try}
import json._

object ResumeTest extends App {
  def doIt(): Unit = {
    val fp = Paths.get("/Users/jake/Documents/Projects/Libraries/hcl4s", "examples/resume.hcl")
    val op = Paths.get("/Users/jake/Documents/Projects/Libraries/hcl4s", "examples/resume.json")

    val parsed = HCL.fromFile(fp, blockNaming = None)
    val body = parsed match {
      case Left(err) => throw err
      case Right(value) => value
    }

    val obj = body.toObject

    val j = obj.asJson

    Files.write(op, j.toString().getBytes)
  }

  Try(doIt()) match {
    case Failure(err:HCLError) =>
      println(s"\u001b[31mProcessed failed with error of type ${err.getClass.getSimpleName}\u001b[0m")
      println(err.getMessage)
    case Failure(f) => throw f
    case Success(_) => ()
  }
}