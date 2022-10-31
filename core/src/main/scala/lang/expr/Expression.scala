package dev.jtrim777.hcl4s.lang.expr

import dev.jtrim777.hcl4s.lang.DataType
import dev.jtrim777.hcl4s.lang.expr.operators._
import dev.jtrim777.hcl4s.lang.struct.{Block, ResolvedBlock}
import dev.jtrim777.hcl4s.lang.tmpl.Template
import dev.jtrim777.hcl4s.util.Traceable

sealed trait Expression extends Traceable

object Expression {
  sealed trait Term extends Expression
  sealed trait AbsoluteTerm extends Term {
    def kind: DataType
    def stringify: String
  }

  case class Literal(value: ValueType[_]) extends AbsoluteTerm {
    override def kind: DataType = value.dataType
    override def stringify: String = value.value.toString

    override def traceType: String = "literal"
    override def traceDisplay: String = stringify
    override def shortDisplay: String = stringify
  }
  case class AttrKey(value: String) extends AbsoluteTerm {
    override def kind: DataType = DataType.Text

    override def stringify: String = value

    override def traceType: String = "identifier"
    override def traceDisplay: String = stringify
    override def shortDisplay: String = stringify
  }

  case class BlockRef(block: ResolvedBlock) extends AbsoluteTerm {
    override def kind: DataType = DataType.NotAType
    override def stringify: String = "&" + block.kind + (if (block.labels.nonEmpty) block.labels.mkString(".", ".", "") else "")

    override def traceType: String = "block_reference"

    override def traceDisplay: String = stringify
    override def shortDisplay: String = stringify
  }

  sealed trait Collection extends Term
  case class SequenceT(items: List[Expression]) extends Collection {
    override def traceType: String = "sequence"
    override def traceDisplay: String = items.map(_.traceDisplay).mkString("[", ", ", "]")
    override def shortDisplay: String = if (items.nonEmpty) s"[${items.head.shortDisplay},...]" else s"[]"
  }
  case class MappingT(items: Map[Expression, Expression]) extends Collection {
    override def traceType: String = "mapping"
    override def traceDisplay: String = items.map(p => p._1.traceDisplay + ": " + p._2.traceDisplay).mkString("{", ", ", "}")
    override def shortDisplay: String = if (items.nonEmpty) s"{${items.head._1.shortDisplay}: ${items.head._2.shortDisplay},...}" else s"[]"
  }

  sealed trait AbsoluteCollection extends AbsoluteTerm
  case class AbsSequence(items: List[AbsoluteTerm]) extends AbsoluteCollection {
    override def kind: DataType = DataType.Sequential

    override def stringify: String = traceDisplay

    override def traceType: String = "sequence"
    override def traceDisplay: String = items.map(_.traceDisplay).mkString("[", ", ", "]")
    override def shortDisplay: String = if (items.nonEmpty) s"[${items.head.shortDisplay},...]" else s"[]"
  }
  case class AbsMapping(items: Map[String, AbsoluteTerm]) extends AbsoluteCollection {
    override def kind: DataType = DataType.ValueMap

    override def stringify: String = traceDisplay

    override def traceType: String = "mapping"
    override def traceDisplay: String = items.map(p => p._1 + ": " + p._2.traceDisplay).mkString("{", ", ", "}")
    override def shortDisplay: String = if (items.nonEmpty) s"{${items.head._1}: ${items.head._2.shortDisplay},...}" else s"[]"

    def add(path: List[String], value: AbsoluteTerm): AbsMapping = {
      if (path.isEmpty) {
        throw new IllegalArgumentException("Cannot add value with empty key path")
      } else if (path.length == 1) {
        this.copy(items = items.updated(path.head, value))
      } else {
        items.get(path.head) match {
          case Some(m:AbsMapping) => this.copy(items = items.updated(path.head, m.add(path.tail, value)))
          case None => this.copy(items = items.updated(path.head, AbsMapping(Map.empty).add(path.tail, value)))
        }
      }
    }
  }

  case class Variable(id: String) extends Term {
    override def traceType: String = "variable"
    override def traceDisplay: String = id
    override def shortDisplay: String = id
  }

  case class TmplExpr(template: Template) extends Term {
    override def traceType: String = "template"
    override def traceDisplay: String = template.traceDisplay
    override def shortDisplay: String = template.shortDisplay
  }

  case class ResolvedTmpl(value: String) extends AbsoluteTerm {
    override def kind: DataType = DataType.Text
    override def stringify: String = value

    override def traceType: String = "string"
    override def traceDisplay: String = value
    override def shortDisplay: String = "\"...\""
  }

  case class FuncCall(func: String, args: List[Expression]) extends Term {
    override def traceType: String = "function call"
    override def traceDisplay: String = s"$func(${args.map(_.traceDisplay).mkString(", ")})"
    override def shortDisplay: String = s"$func(...)"
  }

  sealed trait ForExpr extends Term
  case class ForSeqExpr(primID: String, secID: Option[String],
                        seqExpr: Expression, valExpr: Expression, cond: Option[Expression]) extends ForExpr {
    override def traceType: String = "for comprehension"
    override def traceDisplay: String = s"[for $primID${secID.map(s => ", " + s).getOrElse("")} in " +
      s"${seqExpr.traceDisplay} : ${valExpr.traceDisplay}${cond.map(s => " if " + s.traceDisplay).getOrElse("")}]"
    override def shortDisplay: String = s"[for $primID${secID.map(s => ", " + s).getOrElse("")} in " +
      s"${seqExpr.shortDisplay} : ...${cond.map(_ => " if ...").getOrElse("")}]"
  }

  case class ForMapExpr(primID: String, secID: Option[String], seqExpr: Expression, keyExpr: Expression,
                        valExpr: Expression, group: Boolean, cond: Option[Expression]) extends ForExpr {
    override def traceType: String = "map comprehension"
    override def traceDisplay: String = s"{for $primID${secID.map(s => ", " + s).getOrElse("")} in " +
      s"${seqExpr.traceDisplay} : ${keyExpr.traceDisplay} => ${valExpr.traceDisplay}${cond.map(s => " if " + s.traceDisplay).getOrElse("")}}"
    override def shortDisplay: String = s"{for $primID${secID.map(s => ", " + s).getOrElse("")} in " +
      s"${seqExpr.shortDisplay} : ... => ...${cond.map(_ => " if ...").getOrElse("")}}"
  }

  sealed trait AccessTerm extends Term {
    val target: Expression
  }
  case class Index(target: Expression, index: Expression) extends AccessTerm {
    override def traceType: String = "index"
    override def traceDisplay: String = s"${target.traceDisplay}[${index.traceDisplay}]"
    override def shortDisplay: String = s"...[${index.shortDisplay}]"
  }
  case class GetAttr(target: Expression, key: String) extends AccessTerm {
    override def traceType: String = "attribute access"
    override def traceDisplay: String = s"${target.traceDisplay}.$key"
    override def shortDisplay: String = s"${target.shortDisplay}.$key"
  }

  case class AttrSplat(target: Expression, ext: List[String]) extends Term {
    override def traceType: String = "attribute splat"
    override def traceDisplay: String = s"${target.traceDisplay}.*.${ext.mkString(".")}"
    override def shortDisplay: String = s"${target.shortDisplay}.*.(...)"
  }
  case class FullSplat(target: Expression, ext: List[Either[String, Expression]]) extends Term {
    override def traceType: String = "full splat"
    override def traceDisplay: String = {
      val axes = ext.map {
        case Right(value) => s"[${value.traceDisplay}]"
        case Left(value) => s".$value"
      }.mkString("")
      s"${target.traceDisplay}[*]$axes"
    }
    override def shortDisplay: String = s"${target.shortDisplay}[*]..."
  }


  sealed trait Operation extends Expression
  case class UnaryOp(op: UnaryOperator, target: Term) extends Operation {
    override def traceType: String = "operation"
    override def traceDisplay: String = s"${op.id}${target.traceDisplay}"
    override def shortDisplay: String = s"${op.id}${target.shortDisplay}"
  }
  case class BinaryOp(lhs: Term, op: BinaryOperator[_,_], rhs: Term) extends Operation {
    override def traceType: String = "operation"
    override def traceDisplay: String = s"${lhs.traceDisplay}${op.id}${rhs.traceDisplay}"
    override def shortDisplay: String = s"${rhs.shortDisplay}${op.id}${rhs.shortDisplay}"
  }

  case class Conditional(cond: Term, yesVal: Expression, noVal: Expression) extends Expression {
    override def traceType: String = "conditional"
    override def traceDisplay: String = s"${cond.traceDisplay} ? ${yesVal.traceDisplay} : ${noVal.traceDisplay}"
    override def shortDisplay: String = s"${cond.shortDisplay} ? ... : ..."
  }

  case class WrappedExpr(expr: Expression) extends Term {
    override def traceType: String = expr.traceType

    override def traceDisplay: String = s"(${expr.traceDisplay})"

    override def shortDisplay: String = s"(${expr.shortDisplay})"
  }


}
