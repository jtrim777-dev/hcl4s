package dev.jtrim777.hcl4s.generic

import dev.jtrim777.hcl4s.codec.{ValueDecoder, ValueEncoder}
import dev.jtrim777.hcl4s.model.HCLValue
import io.circe.generic.decoding.DerivedDecoder
import io.circe.generic.encoding.DerivedAsObjectEncoder
import io.circe.{Json, Decoder => CirceDecoder, Encoder => CirceEncoder}
import io.circe.generic.{semiauto => cdev}

object semiauto {
  implicit def HCLForCirceEnc[A : CirceEncoder]: ValueEncoder[A] = new ValueEncoder[A] {
    override def encodeValue(a: A): HCLValue = {
      val circeEncoded = implicitly[CirceEncoder[A]].apply(a)
      jsonToHCL(circeEncoded)
    }
  }

  implicit def HCLForCirceDec[A : CirceDecoder]: ValueDecoder[A] = new ValueDecoder[A] {
    override def decodeValue(v: HCLValue): A = {
      val decoded = implicitly[CirceDecoder[A]].decodeJson(hclToJson(v))
      decoded match {
        case Left(err) => throw err
        case Right(value) => value
      }
    }
  }

  def deriveEncoder[A : DerivedAsObjectEncoder]: ValueEncoder[A] = {
    implicit val circeX: CirceEncoder[A] = cdev.deriveEncoder[A]
    implicitly[ValueEncoder[A]]
  }

  def deriveDecoder[A : DerivedDecoder]: ValueDecoder[A] = {
    implicit val circeX: CirceDecoder[A] = cdev.deriveDecoder[A]
    implicitly[ValueDecoder[A]]
  }

  private def jsonToHCL(json: Json): HCLValue = {
    if (json.isNull) {
      HCLValue.Null
    } else if (json.isBoolean) {
      HCLValue.Bool(json.asBoolean.get)
    } else if (json.isNumber) {
      HCLValue.Number(json.asNumber.get.toDouble)
    } else if (json.isString) {
      HCLValue.Text(json.asString.get)
    } else if (json.isArray) {
      val values = json.asArray.get.toList.map(j => jsonToHCL(j))
      HCLValue.HCLList(values)
    } else {
      val map = json.asObject.get.toMap.view.mapValues(j => jsonToHCL(j)).toMap
      HCLValue.HCLObject(map)
    }
  }

  private def hclToJson(value: HCLValue): Json = value match {
    case HCLValue.Number(value) => Json.fromDouble(value).get
    case HCLValue.Bool(value) => Json.fromBoolean(value)
    case HCLValue.Text(value) => Json.fromString(value)
    case HCLValue.Null => Json.Null
    case HCLValue.HCLList(values) => Json.fromValues(values.map(hclToJson))
    case HCLValue.HCLObject(data) => Json.fromFields(data.toList.map(p => (p._1, hclToJson(p._2))))
    case HCLValue.BlockReference(path) => Json.fromString(path.mkString("."))
  }
}
