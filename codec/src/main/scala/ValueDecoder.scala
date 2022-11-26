package dev.jtrim777.hcl4s.codec

import dev.jtrim777.hcl4s.model.HCLValue

trait ValueDecoder[T] {
  def decodeValue(v: HCLValue): T
}
