package dev.jtrim777.hcl4s.model

sealed trait HCLValue {

}

object HCLValue {
  case class Number(value: Double) extends HCLValue
  case class Bool(value: Boolean) extends HCLValue
  case class Text(value: String) extends HCLValue
  case object Null extends HCLValue

  case class HCLList(values: List[HCLValue]) extends HCLValue
  case class HCLObject(data: Map[String, HCLValue]) extends HCLValue
}
