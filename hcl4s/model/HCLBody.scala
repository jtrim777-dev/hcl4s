package dev.jtrim777.hcl4s.model

case class HCLBody(attributes: Map[String, HCLValue], blocks: List[HCLBlock])
