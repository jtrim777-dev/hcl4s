package dev.jtrim777.hcl4s.codec

import dev.jtrim777.hcl4s.model.{HCLBlock, HCLBody, HCLValue}

object dsl {
  def hcl(content: Either[(String, HCLValue), HCLBlock]*): HCLBody = {
    val attrs = content.collect({ case Left(pair) => pair }).toMap
    val blocks = content.collect { case Right(block) => block }

    HCLBody(attrs, blocks.toList)
  }

  def block(kind: String, labels: String*)
           (content: Either[(String, HCLValue), HCLBlock]*): Either[(String, HCLValue), HCLBlock] = {
    Right(HCLBlock(kind, labels.toList, hcl(content:_*)))
  }

  def blockOf[T : Encoder](body: T)(kind: String, labels: String*): Either[(String, HCLValue), HCLBlock] = {
    Right(HCLBlock(kind, labels.toList, body.asHCL))
  }

  implicit class StrExt(val str: String) {
    def :=[T : ValueEncoder](value: T): Either[(String, HCLValue), HCLBlock] =
      Left((str, value.asHCLValue))
  }
}
