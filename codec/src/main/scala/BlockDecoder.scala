package dev.jtrim777.hcl4s.codec

import dev.jtrim777.hcl4s.model.{HCLBlock, HCLBody, HCLValue}

trait BlockDecoder[T]  {
  def decode(hcl: HCLBlock): T
}
