package dev.jtrim777.hcl4s.json

import dev.jtrim777.hcl4s.model.HCLValue
import io.circe.Decoder.Result
import io.circe.{Codec, DecodingFailure, Encoder, HCursor, Json}

object Hcl4sJson {
  implicit val NumCodec: Codec[HCLValue.Number] = new Codec[HCLValue.Number] {
    override def apply(c: HCursor): Result[HCLValue.Number] = {
      c.as[Double].map(HCLValue.Number)
    }

    override def apply(a: HCLValue.Number): Json = Json.fromDoubleOrNull(a.value)
  }

  implicit val BoolCodec: Codec[HCLValue.Bool] = new Codec[HCLValue.Bool] {
    override def apply(c: HCursor): Result[HCLValue.Bool] =
      c.as[Boolean].map(HCLValue.Bool)

    override def apply(a: HCLValue.Bool): Json = Json.fromBoolean(a.value)
  }

  implicit val TxtCodec: Codec[HCLValue.Text] = new Codec[HCLValue.Text] {
    override def apply(c: HCursor): Result[HCLValue.Text] =
      c.as[String].map(HCLValue.Text)

    override def apply(a: HCLValue.Text): Json = Json.fromString(a.value)
  }
  implicit val NullCodec: Encoder[HCLValue.Null.type] = (_: HCLValue.Null.type) => Json.Null

  implicit val ListCodec: Codec[HCLValue.HCLList] = new Codec[HCLValue.HCLList] {
    override def apply(c: HCursor): Result[HCLValue.HCLList] = {
      c.as[List[HCLValue]].map(ls => HCLValue.HCLList(ls))
    }

    override def apply(a: HCLValue.HCLList): Json = Json.arr(a.values.map(_.asJson):_*)
  }

  implicit val ObjCodec: Codec[HCLValue.HCLObject] = new Codec[HCLValue.HCLObject] {
    override def apply(c: HCursor): Result[HCLValue.HCLObject] =
      c.as[Map[String, HCLValue]].map(d => HCLValue.HCLObject(d))

    override def apply(a: HCLValue.HCLObject): Json = {
      val pairs = a.data.toList.map(p => (p._1, p._2.asJson))

      Json.obj(pairs:_*)
    }
  }

  implicit val RefCodec: Encoder[HCLValue.BlockReference] = (a: HCLValue.BlockReference) => {
    Json.obj("ref" -> Json.fromString(a.path.mkString(".")))
  }

  implicit val ValueCodec: Codec[HCLValue] = new Codec[HCLValue] {
    override def apply(c: HCursor): Result[HCLValue] = {
      val value = c.value

      if (value == Json.Null) {
        Right(HCLValue.Null)
      } else if (value.isArray) {
        ListCodec(c)
      } else if (value.isObject) {
        ObjCodec(c)
      } else if (value.isNumber) {
        NumCodec(c)
      } else if (value.isBoolean) {
        BoolCodec(c)
      } else if (value.isString) {
        TxtCodec(c)
      } else {
        Left(DecodingFailure(s"Cannot turn unknown value '${value}' into HCL", List.empty))
      }
    }

    override def apply(a: HCLValue): Json = a match {
      case n:HCLValue.Number => NumCodec(n)
      case b:HCLValue.Bool => BoolCodec(b)
      case t:HCLValue.Text => TxtCodec(t)
      case HCLValue.Null => Json.Null
      case l:HCLValue.HCLList => ListCodec(l)
      case o:HCLValue.HCLObject => ObjCodec(o)
      case r:HCLValue.BlockReference => RefCodec(r)
    }
  }
}