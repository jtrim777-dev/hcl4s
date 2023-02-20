package dev.jtrim777.hcl4s
package generic

import codec.{ValueDecoder, ValueEncoder}

import shapeless.{LabelledGeneric, Lazy}

object auto extends Derivations {
  implicit def genericEncoder[A, H](implicit generic: LabelledGeneric.Aux[A, H],
                                                     hcoder: Lazy[ValueEncoder[H]]): ValueEncoder[A] = { a => hcoder.value.encodeValue(generic.to(a)) }

  implicit def genericDecoder[A, H](implicit generic: LabelledGeneric.Aux[A, H],
                                                     hcoder: Lazy[ValueDecoder[H]]): ValueDecoder[A] = { e =>
    generic.from(hcoder.value.decodeValue(e))
  }
}
