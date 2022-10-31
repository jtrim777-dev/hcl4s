package dev.jtrim777.hcl4s.lang

import dev.jtrim777.hcl4s.lang.expr.Expression
import dev.jtrim777.hcl4s.lang.struct.BodyElemT.{AttributeT, BlockT}

package object struct {
  type Attribute = AttributeT[Expression]
  type Block = BlockT[Expression]

  type ResolvedAttribute = AttributeT[Expression.AbsoluteTerm]
  type ResolvedBlock = BlockT[Expression.AbsoluteTerm]

  type BodyElem = BodyElemT[Expression]
  type ResolvedBody = BodyElemT[Expression.AbsoluteTerm]

  type HCLSource = HCLSourceT[Expression]
  type ResolvedHCL = HCLSourceT[Expression.AbsoluteTerm]
}
