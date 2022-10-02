package dev.jtrim777.hcl4s.eval

import dev.jtrim777.hcl4s.lang.expr.Expression.ResolvedTmpl
import dev.jtrim777.hcl4s.lang.expr.{Expression, ValueType}
import dev.jtrim777.hcl4s.lang.tmpl.{Template, TemplateItem}

object TmplEval {
  def evaluateTemplate(tmpl: Template, ctx: Context): ResolvedTmpl = {
    val items = tmpl.content.toArray
    ResolvedTmpl(items.indices.map { i =>
      evalItem(if (i>0) Some(items(i-1)) else None, items(i), if (i<items.length-1) Some(items(i+1)) else None, ctx.push(items(i)))
    }.mkString(""))
  }

  def evalItem(before: Option[TemplateItem], item: TemplateItem, after: Option[TemplateItem], ctx: Context): String = {
    item match {
      case TemplateItem.Literal(value) =>
        val v1 = if (before.exists(_.stripEnd)) value.stripLeading() else value
        if (after.exists(_.stripStart)) value.stripTrailing() else value
      case TemplateItem.Interpolation(_, value, _) => ExprEval.evaluateExpression(value, ctx.push(value)).stringify
      case directive: TemplateItem.Directive => directive match {
        case TemplateItem.TmplIf(_, cond, resolve, alt, _) =>
          val condVal = ExprEval.evaluateExpression(cond, ctx.push(cond))
          condVal match {
            case Expression.Literal(ValueType.BooleanValue(flag)) => if (flag) {
              evaluateTemplate(resolve, ctx.push(resolve)).stringify
            } else alt.map(t => evaluateTemplate(t, ctx.push(t)).stringify).getOrElse("")
            case _ => ctx.throwError("Condition must resolve to a boolean")
          }
        case TemplateItem.TmplFor(_, primID, secID, seqExpr, resolve, _) => evalForDirective(primID, secID, seqExpr, resolve, ctx)
      }
    }
  }

  private def evalForDirective(primID: String, secID: Option[String],
                               seqExpr: Expression, resolve: Template, ctx: Context): String = {
    val seqColl = ExprEval.evaluateExpression(seqExpr, ctx.push(seqExpr)) match {
      case coll:Expression.AbsoluteCollection => coll
      case _ => ctx.throwError("Target expression in for directive must resolve to a collection")
    }

    seqColl match {
      case Expression.AbsSequence(values) =>
        val nvals = values.zipWithIndex.map { case (term, i) =>
          val nctx = secID match {
            case Some(secID) =>
              ctx.push(resolve)
                .enscope(primID, Expression.Literal(ValueType.IntegerValue(i)))
                .enscope(secID, term)
            case None => ctx.push(resolve)
              .enscope(primID, term)
          }
          evaluateTemplate(resolve, nctx).value
        }
        nvals.mkString("")
      case Expression.AbsMapping(values) =>
        val nvals = values.toList.map { case (key, value) =>
          val nctx = secID match {
            case Some(secID) =>
              ctx.push(resolve)
                .enscope(primID, key)
                .enscope(secID, value)
            case None => ctx.push(resolve)
              .enscope(primID, value)
          }
          evaluateTemplate(resolve, nctx).value
        }
        nvals.mkString("")
    }
  }

  def keysUsed(set: Set[String], tmpl: Template): Set[String] = tmpl.content.flatMap {
    case TemplateItem.Literal(_) => Set.empty
    case TemplateItem.Interpolation(_, value, _) => ExprEval.keysUsed(set, value)
    case directive: TemplateItem.Directive => directive match {
      case TemplateItem.TmplIf(_, cond, resolve, alt, _) =>
        ExprEval.keysUsed(set, cond) ++ keysUsed(set, resolve) ++ alt.map(keysUsed(set, _)).getOrElse(Set.empty)
      case TemplateItem.TmplFor(_, primID, secID, seqExpr, resolve, _) =>
        val ignore = Set(primID) ++ secID.map(Set(_)).getOrElse(Set.empty)
        ExprEval.keysUsed(set, seqExpr) ++ (keysUsed(set, resolve) -- ignore)
    }
  }.toSet
}
