package dev.jtrim777.hcl4s.lang

sealed trait DataType {

}

object DataType {
  case object Numeric extends DataType
  case object Text extends DataType
  case object Sequential extends DataType
  case object ValueMap extends DataType
  case object Bool extends DataType
  case object NotAType extends DataType
}
