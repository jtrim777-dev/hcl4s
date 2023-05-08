package dev.jtrim777.hcl4s

import model.HCLValue

import io.circe.{Decoder, Json}

package object json {
  implicit class JsonValueExt(val tgt: HCLValue) {
    def asJson: Json = Hcl4sJson.ValueCodec.apply(tgt)
  }

  def jsonToHcl(json: Json): Decoder.Result[HCLValue] = Hcl4sJson.ValueCodec.decodeJson(json)
}
