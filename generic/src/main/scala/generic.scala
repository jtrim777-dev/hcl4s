package dev.jtrim777.hcl4s.generic

import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness, Coproduct, CNil, :+:, Inl, Inr}
import shapeless.labelled.{FieldType, field}
import dev.jtrim777.hcl4s.codec._

object generic {

    // case class FooBar(baz: String, qux: Int, quill: EggsSpam) -> foo_bar { baz = ""; quz = 5; eggs_spam "quill" { ... } }

  implicit val hnilCodec: NBTCodec[HNil] = codec(
    { _ => mapCodec[String].encode(Map.empty) },
    { _ => HNil }
  )

  implicit def hlistEncoder[K <: Symbol, H, T <: HList](implicit
                                                        witness: Witness.Aux[K],
                                                        hEncoder: Lazy[Encoder[H]],
                                                        tEncoder: Encoder[T]
                                                       ): Encoder[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name

    { (input: FieldType[K, H] :: T) =>
      val head = hEncoder.value.encode(input.head)
      val tail = tEncoder.encode(input.tail)
      tail.downCast(NbtCompound.TYPE).put(fieldName, head)
    }
  }

  implicit def hlistDecoder[K <: Symbol, H, T <: HList](implicit
                                                        witness: Witness.Aux[K],
                                                        hDecoder: Lazy[ValueDecoder[H]],
                                                        tDecoder: ValueDecoder[T]
                                                       ): ValueDecoder[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name

    { (source: NbtElement) =>
      val cmpd = source.downCast(NbtCompound.TYPE)
      val head = hDecoder.value.decode(cmpd.get(fieldName))
      val tail = tDecoder.decode(source)
      field[K](head) :: tail
    }
  }

//  implicit def hlistOOEncoder[K <: Symbol, H, T <: HList](implicit
//                                                          witness: Witness.Aux[K],
//                                                          hEncoder: Lazy[Encoder[H]],
//                                                          tEncoder: Encoder[T]
//                                                         ): Encoder[FieldType[K, Option[H]] :: T] = {
//    val fieldName = witness.value.name
//
//    { (input: FieldType[K, Option[H]] :: T) =>
//      val tail = tEncoder.encode(input.tail).downCast(NbtCompound.TYPE)
//
//      input.head.foreach(h => tail.put(fieldName, hEncoder.value.encode(h)))
//      tail
//    }
//  }
//
//  implicit def hlistOODecoder[K <: Symbol, H, T <: HList](implicit
//                                                          witness: Witness.Aux[K],
//                                                          hDecoder: Lazy[ValueDecoder[H]],
//                                                          tDecoder: ValueDecoder[T]
//                                                         ): ValueDecoder[FieldType[K, Option[H]] :: T] = {
//    val fieldName = witness.value.name
//
//    { (source: NbtElement) =>
//      val cmpd = source.downCast(NbtCompound.TYPE)
//      val head = if (cmpd.contains(fieldName)) Some(hDecoder.value.decode(cmpd.get(fieldName))) else None
//      val tail = tDecoder.decode(source)
//      field[K](head) :: tail
//    }
//  }


  implicit def genericEncoder[A, H](implicit generic: LabelledGeneric.Aux[A, H],
                                    hcoder: Lazy[Encoder[H]]): Encoder[A] = { a => hcoder.value.encode(generic.to(a)) }

  implicit def genericDecoder[A, H](implicit generic: LabelledGeneric.Aux[A, H],
                                    hcoder: Lazy[ValueDecoder[H]]): ValueDecoder[A] = { e =>
    generic.from(hcoder.value.decode(e))
  }
}
