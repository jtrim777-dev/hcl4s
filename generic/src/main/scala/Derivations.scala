package dev.jtrim777.hcl4s.generic

import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Inl, Inr, LabelledGeneric, Lazy, Witness}
import shapeless.labelled.{FieldType, field}
import dev.jtrim777.hcl4s.codec._
import dev.jtrim777.hcl4s.model.HCLValue
import dev.jtrim777.hcl4s.model.HCLValue.HCLObject

trait Derivations {
  private[generic] implicit val hnilEncoder: ValueEncoder[HNil] = (_) => MapEncoder[HCLValue].encodeValue(Map.empty[String, HCLValue])
  private[generic] implicit val hnilDecoder: ValueDecoder[HNil] = (_) => HNil

  private[generic] implicit def hlistEncoder[K <: Symbol, H, T <: HList](implicit
                                                        witness: Witness.Aux[K],
                                                        hEncoder: Lazy[ValueEncoder[H]],
                                                        tEncoder: ValueEncoder[T]
                                                       ): ValueEncoder[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name

    { (input: FieldType[K, H] :: T) =>
      val head = hEncoder.value.encodeValue(input.head)
      val tail = tEncoder.encodeValue(input.tail)
      tail.asInstanceOf[HCLValue.HCLObject].put(fieldName, head)
    }
  }

  private[generic] implicit def hlistDecoder[K <: Symbol, H, T <: HList](implicit
                                                        witness: Witness.Aux[K],
                                                        hDecoder: Lazy[ValueDecoder[H]],
                                                        tDecoder: ValueDecoder[T]
                                                       ): ValueDecoder[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name

    { (source: HCLValue) =>
      val cmpd = source.asInstanceOf[HCLObject]
      val head = hDecoder.value.decodeValue(cmpd.data(fieldName))
      val tail = tDecoder.decodeValue(source)
      field[K](head) :: tail
    }
  }
  //
  ////  implicit def hlistOOEncoder[K <: Symbol, H, T <: HList](implicit
  ////                                                          witness: Witness.Aux[K],
  ////                                                          hEncoder: Lazy[Encoder[H]],
  ////                                                          tEncoder: Encoder[T]
  ////                                                         ): Encoder[FieldType[K, Option[H]] :: T] = {
  ////    val fieldName = witness.value.name
  ////
  ////    { (input: FieldType[K, Option[H]] :: T) =>
  ////      val tail = tEncoder.encode(input.tail).downCast(NbtCompound.TYPE)
  ////
  ////      input.head.foreach(h => tail.put(fieldName, hEncoder.value.encode(h)))
  ////      tail
  ////    }
  ////  }
  ////
  ////  implicit def hlistOODecoder[K <: Symbol, H, T <: HList](implicit
  ////                                                          witness: Witness.Aux[K],
  ////                                                          hDecoder: Lazy[ValueDecoder[H]],
  ////                                                          tDecoder: ValueDecoder[T]
  ////                                                         ): ValueDecoder[FieldType[K, Option[H]] :: T] = {
  ////    val fieldName = witness.value.name
  ////
  ////    { (source: NbtElement) =>
  ////      val cmpd = source.downCast(NbtCompound.TYPE)
  ////      val head = if (cmpd.contains(fieldName)) Some(hDecoder.value.decode(cmpd.get(fieldName))) else None
  ////      val tail = tDecoder.decode(source)
  ////      field[K](head) :: tail
  ////    }
  ////  }
  //
  //
}

