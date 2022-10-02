package dev.jtrim777.hcl4s.lang.struct

import dev.jtrim777.hcl4s.lang.expr.Expression

case class HCLSourceT[T <: Expression](elements: List[BodyElemT[T]])
