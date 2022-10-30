package dev.jtrim777.hcl4s.codec

import dev.jtrim777.hcl4s.model.HCLValue

trait ValueEncoder[-T] {
  def encodeValue(a: T): HCLValue
}
