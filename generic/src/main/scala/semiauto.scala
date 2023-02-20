package dev.jtrim777.hcl4s.generic

import dev.jtrim777.hcl4s.codec._
import dev.jtrim777.hcl4s.model.HCLValue
import dev.jtrim777.hcl4s.model.HCLValue.HCLObject

object semiauto {
  import auto._

  def deriveEncoder[A]: ValueEncoder[A] = implicitly[ValueEncoder[A]]
}
