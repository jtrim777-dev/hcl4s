package dev.jtrim777.hcl4s.parser

import fastparse._, JavaWhitespace._
import FastparseUtils._

private[parser] object IDHelpers {
  def CoreID[_: P]: Rule0 = P {
    ("_" | CharAlpha) ~~ (CharAlphaNum | "_" | "$").repX
  }

  def ID[_: P]: Rule1[String] = P {
    CoreID.! ~ !(CharAlphaNum | "_" | "$")
  }
}
