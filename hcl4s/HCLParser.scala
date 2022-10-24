package dev.jtrim777.hcl4s

import dev.jtrim777.hcl4s.eval.HCLEval
import dev.jtrim777.hcl4s.lang.struct.HCLSource
import dev.jtrim777.hcl4s.model.HCLValue
import lang.dsl._

object HCLParser extends App {
  val test: HCLSource = hcl(
    "rando" := 72,
    block("target", "ci-corretto-17")(
      "inherits" := seq("ci-corretto-base"),
      "tags" := seq(
        "circleci/corretto:8",
        tmpl(interpolate("registry".asVar) ~ "/".asLit ~ interpolate("repo".asVar) ~ "/corretto:11".asLit)
      ),
      "args" := obj(
        "JDK_VERSION" ~> 17
      )
    )
  )

  pprint.pprintln(test)

  val result = HCLEval.evaluate(test, Map(
    "registry" -> HCLValue.Text("598748548552.dkr.ecr.us-west-2.amazonaws.com"),
    "repo" -> HCLValue.Text("build-images")
  ))

  pprint.pprintln(result)
}
