package dev.jtrim777.hcl4s

import dev.jtrim777.hcl4s.model.{HCLBody, HCLValue}

package object codec {

  implicit class OnValuable[T : ValueEncoder](obj: T) {
    def asHCLValue: HCLValue = implicitly[ValueEncoder[T]].encodeValue(obj)
  }

  implicit class OnEncodable[T: Encoder](obj: T) {
    def asHCLValue: HCLValue = implicitly[Encoder[T]].encodeValue(obj)
    def asHCL: HCLBody = implicitly[Encoder[T]].encode(obj)
  }

  implicit val SelfEncoder: ValueEncoder[HCLValue] = (a: HCLValue) => a
  implicit val StrEncoder: ValueEncoder[String] = (a: String) => HCLValue.Text(a)
  implicit val IntEncoder: ValueEncoder[Int] = (a: Int) => HCLValue.Number(a)
  implicit val LongEncoder: ValueEncoder[Long] = (a: Long) => HCLValue.Number(a)
  implicit val FloatEncoder: ValueEncoder[Float] = (a: Float) => HCLValue.Number(a)
  implicit val DoubleEncoder: ValueEncoder[Double] = (a: Double) => HCLValue.Number(a)
  implicit val BoolEncoder: ValueEncoder[Boolean] = (a: Boolean) => HCLValue.Bool(a)
  implicit val NullEncoder: ValueEncoder[Null] = (_: Null) => HCLValue.Null

  implicit def IterEncoder[T : ValueEncoder]: ValueEncoder[Iterable[T]] = { (seq: Iterable[T]) =>
    val encoder = implicitly[ValueEncoder[T]]
    HCLValue.HCLList(seq.toList.map(t => encoder.encodeValue(t)))
  }

  implicit def MapEncoder[T : ValueEncoder]: ValueEncoder[Map[String, T]] = { (obj: Map[String, T]) =>
    val encoder = implicitly[ValueEncoder[T]]
    val coded = obj.map(t => (t._1, encoder.encodeValue(t._2)))

    HCLValue.HCLObject(coded)
  }
}
