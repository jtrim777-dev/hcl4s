package dev.jtrim777.hcl4s.generic

import dev.jtrim777.hcl4s.codec.{ValueDecoder, ValueEncoder}
import shapeless.{LabelledGeneric, Lazy}

object assist {
  abstract class DerivedEncoder[A] extends ValueEncoder[A]
  abstract class DerivedDecoder[A] extends ValueDecoder[A]

  object DerivedEncoder {
    implicit def deriveEncNow[A, R](implicit gen: LabelledGeneric.Aux[A, R], enc: Lazy[])
  }
}
