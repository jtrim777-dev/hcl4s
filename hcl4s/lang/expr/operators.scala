package dev.jtrim777.hcl4s.lang.expr

import dev.jtrim777.hcl4s.lang.DataType

object operators {
  sealed abstract class UnaryOperator(val id: String)
  case object Negate extends UnaryOperator("-")
  case object Not extends UnaryOperator("!")

  sealed abstract class BinaryOperator[T, O](val id: String, val precedence: Int, val inputKind: DataType, val outputKind: DataType) {
    def doIt(a: T, b: T): O
  }
  
  sealed abstract class CompOp(id: String, precedence: Int, doit: (Double,Double) => Boolean) 
    extends BinaryOperator[Double, Boolean](id, precedence, DataType.Numeric, DataType.Bool) {
    override def doIt(a: Double, b: Double): Boolean = doit(a, b)
  }
  case object Equals extends CompOp("==", 3, _ == _)
  case object NotEquals extends CompOp("!=", 3, _ != _)
  case object LessThan extends CompOp("<", 3, _ < _)
  case object GreaterThan extends CompOp(">", 4, _ > _)
  case object LessOrEq extends CompOp("<=", 4, _ <= _)
  case object GreaterOrEq extends CompOp(">=", 4, _ >= _)

  sealed abstract class ArithOp(id: String, precedence: Int, doit: (Double, Double) => Double)
    extends BinaryOperator[Double, Double](id, precedence, DataType.Numeric, DataType.Numeric) {
    override def doIt(a: Double, b: Double): Double = doit(a,b)
  }
  case object Add extends ArithOp("+", 5, _ + _)
  case object Subtract extends ArithOp("-", 5, _ - _)
  case object Multiply extends ArithOp("*", 6, _ * _)
  case object Divide extends ArithOp("/", 6, _ / _)
  case object Modulo extends ArithOp("%", 6, _ % _)

  sealed abstract class LogicalOp(id: String, precedence: Int, doit: (Boolean, Boolean) => Boolean)
    extends BinaryOperator[Boolean, Boolean](id, precedence, DataType.Bool, DataType.Bool) {
    override def doIt(a: Boolean, b: Boolean): Boolean = doit(a,b)
  }
  case object And extends LogicalOp("&&", 2, _ && _)
  case object Or extends LogicalOp("||", 1, _ || _)

  val Binaries: List[BinaryOperator[_, _]] = List(
    Equals, NotEquals, LessThan, LessOrEq, GreaterThan, GreaterOrEq,
    Add, Subtract, Multiply, Divide, Modulo,
    And, Or
  )
}
