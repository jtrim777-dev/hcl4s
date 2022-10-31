package dev.jtrim777.hcl4s.codec

import dev.jtrim777.hcl4s.model.{HCLBody, HCLValue}

trait Encoder[-T] extends ValueEncoder[T] {
  def encode(a: T): HCLBody

  override def encodeValue(a: T): HCLValue = encode(a).toObject
}
