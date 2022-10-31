package dev.jtrim777.hcl4s.lang.expr

import dev.jtrim777.hcl4s.lang.DataType

sealed trait ValueType[T] {
  val value: T
  val dataType: DataType
}

object ValueType {
  sealed trait NumericValue[T] extends ValueType[T] {
    override val dataType: DataType = DataType.Numeric
    val asDouble: Double
  }
  case class IntegerValue(value: Long) extends NumericValue[Long] {
    override val asDouble: Double = value.toDouble
  }
  case class FloatingValue(value: Double) extends NumericValue[Double] {
    override val asDouble: Double = value
  }

  case class BooleanValue(value: Boolean) extends ValueType[Boolean] {
    override val dataType: DataType = DataType.Bool
  }
  case object NullValue extends ValueType[Any] {
    override val value: Any = null
    override val dataType: DataType = DataType.NotAType
  }
}
