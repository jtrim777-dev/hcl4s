package dev.jtrim777.hcl4s.eval

import dev.jtrim777.hcl4s.lang.expr.Expression.{AbsoluteCollection, AbsoluteTerm, AttrKey, WrappedExpr}
import dev.jtrim777.hcl4s.lang.expr.operators.{BinaryOperator, UnaryOperator}
import dev.jtrim777.hcl4s.lang.expr.{Expression, ValueType, operators}

private[eval] object ExprEval {
  def evaluateExpression(expr: Expression, ctx: Context): AbsoluteTerm = expr match {
    case WrappedExpr(wrapped) => evaluateExpression(wrapped, ctx.push(wrapped))
    case term: AbsoluteTerm => term
    case operation: Expression.Operation => operation match {
      case Expression.UnaryOp(op, target) => evalUnaryOperation(op, target, ctx)
      case Expression.BinaryOp(lhs, op, rhs) => evalBinaryOperation(lhs, op, rhs, ctx)
    }
    case Expression.Conditional(cond, yesVal, noVal) => evalConditional(cond, yesVal, noVal, ctx)
    case term: Expression.Term => term match {
      case Expression.Variable(id) => ctx.scope.lookup(id) match {
        case Some(value) => value
        case None => ctx.throwError(s"No such variable $id in scope")
      }
      case collection: Expression.Collection => collection match {
        case Expression.SequenceT(items) => Expression.AbsSequence(items.map(i => evaluateExpression(i, ctx.push(i))))
        case Expression.MappingT(items) => Expression.AbsMapping(items.map { case (k, v) =>
          (asAttrKey(k, ctx).value, evaluateExpression(v, ctx.push(v)))
        })
      }
      case Expression.TmplExpr(template) => TmplEval.evaluateTemplate(template, ctx)
      case Expression.FuncCall(func, args) => evalFuncCall(func, args, ctx)
      case expr: Expression.ForExpr => expr match {
        case fs:Expression.ForSeqExpr =>
          val tgt = evaluateExpression(fs.seqExpr, ctx.push(fs.seqExpr)) match {
            case collection: Expression.AbsoluteCollection => collection
            case _ => ctx.throwError("Input to a for expression must be a collection")
          }
          evalForSeqExpr(tgt, fs, ctx)
        case fm:Expression.ForMapExpr => evalForMapExpr(fm, ctx)
      }
      case term: Expression.AccessTerm =>
        val target = evaluateExpression(term.target, ctx.push(term.target))
        term match {
          case Expression.Index(_, index) => evalIndex(target, index, ctx)
          case Expression.GetAttr(_, key) => evalAttr(target, key, ctx)
        }
      case Expression.AttrSplat(target, ext) =>
        evalAttrSplat(evaluateExpression(target, ctx.push(target)), ext, ctx)
      case Expression.FullSplat(target, ext) =>
        evalFullSplat(evaluateExpression(target, ctx.push(target)), ext, ctx)
    }
  }

  private def evalUnaryOperation(op: UnaryOperator, target: Expression, ctx: Context): AbsoluteTerm = {
    val tt = evaluateExpression(target, ctx.push(target))

    op match {
      case operators.Negate => tt match {
        case Expression.Literal(ValueType.IntegerValue(v)) => Expression.Literal(ValueType.IntegerValue(-v))
        case Expression.Literal(ValueType.FloatingValue(v)) => Expression.Literal(ValueType.FloatingValue(-v))
        case _ => ctx.throwError("The '-' operator only operates on numeric types")
      }
      case operators.Not => tt match {
        case Expression.Literal(ValueType.BooleanValue(v)) => Expression.Literal(ValueType.BooleanValue(!v))
        case _ => ctx.throwError("The '!' operator only operates on boolean types")
      }
    }
  }

  private def evalBinaryOperation(lhs: Expression, op: BinaryOperator[_,_], rhs: Expression, ctx: Context): AbsoluteTerm = {
    val tlhs = evaluateExpression(lhs, ctx.push(lhs))
    val trhs = evaluateExpression(rhs, ctx.push(rhs))

    if (tlhs.kind != op.inputKind || trhs.kind != op.inputKind) {
      ctx.throwError(s"The operation '${op.id}' only operates on ${op.inputKind} operands")
    } else {
      operations.evaluateOperation(op, tlhs, trhs, ctx)
    }
  }

  private def evalConditional(cond: Expression, yesVal: Expression, noVal: Expression, ctx: Context): AbsoluteTerm = {
    val xcond = evaluateExpression(cond, ctx.push(cond))

    val truthy = xcond match {
      case Expression.Literal(value) => value match {
        case value: ValueType.NumericValue[_] => value match {
          case ValueType.IntegerValue(value) => value != 0
          case ValueType.FloatingValue(value) => value != 0
        }
        case ValueType.BooleanValue(value) => value
        case ValueType.NullValue => false
      }
      case AttrKey(value) => value.nonEmpty
      case collection: AbsoluteCollection => collection match {
        case Expression.AbsSequence(items) => items.nonEmpty
        case Expression.AbsMapping(items) => items.nonEmpty
      }
      case Expression.ResolvedTmpl(value) => value.nonEmpty
    }

    if (truthy) {
      evaluateExpression(yesVal, ctx.push(yesVal))
    } else evaluateExpression(noVal, ctx.push(noVal))
  }

  private def evalFuncCall(func: String, args: List[Expression], ctx: Context): AbsoluteTerm = {
    val resolvedArgs = args.map(a => evaluateExpression(a, ctx.push(a)))

    val fx = ctx.funcs.getOrElse(func, ctx.throwError(s"No such function '$func'"))

    fx(resolvedArgs, ctx)
  }

  private def evalForSeqExpr(tgt: Expression.AbsoluteCollection, fs: Expression.ForSeqExpr, ctx: Context): AbsoluteTerm = {
    tgt match {
      case Expression.AbsSequence(values) =>
        val nvals = values.zipWithIndex.flatMap { case (term, i) =>
          val nctx = fs.secID match {
            case Some(secID) =>
              ctx.push(fs.valExpr)
                .enscope(fs.primID, Expression.Literal(ValueType.IntegerValue(i)))
                .enscope(secID, term)
            case None => ctx.push(fs.valExpr)
                .enscope(fs.primID, term)
          }
          val evalCond = fs.cond.map(c => evaluateExpression(c, nctx.push(c))).getOrElse(Expression.Literal(ValueType.BooleanValue(true)))
          evalCond match {
            case Expression.Literal(ValueType.BooleanValue(flag)) => if (flag) {
              Some(evaluateExpression(fs.valExpr, nctx))
            } else None
            case _ => nctx.throwError("Filter in for comprehension must evaluate to a boolean")
          }
        }
        Expression.AbsSequence(nvals)
      case Expression.AbsMapping(values) =>
        val nvals = values.toList.flatMap { case (key, value) =>
          val nctx = fs.secID match {
            case Some(secID) =>
              ctx.push(fs.valExpr)
                .enscope(fs.primID, AttrKey(key))
                .enscope(secID, value)
            case None => ctx.push(fs.valExpr)
              .enscope(fs.primID, value)
          }
          val evalCond = fs.cond.map(c => evaluateExpression(c, nctx.push(c))).getOrElse(Expression.Literal(ValueType.BooleanValue(true)))
          evalCond match {
            case Expression.Literal(ValueType.BooleanValue(flag)) => if (flag) {
              Some(evaluateExpression(fs.valExpr, nctx))
            } else None
            case _ => nctx.throwError("Filter in for comprehension must evaluate to a boolean")
          }
        }
        Expression.AbsSequence(nvals)
    }
  }
  private def evalForMapExpr(fm: Expression.ForMapExpr, ctx: Context): AbsoluteTerm = {
    val evalMap = evaluateExpression(fm.seqExpr, ctx.push(fm.seqExpr))

    val pairs = evalMap match {
      case Expression.AbsSequence(values) =>
        values.zipWithIndex.flatMap { case (value, i) =>
          val nctx = fm.secID match {
            case Some(secID) =>
              ctx.enscope(fm.primID, Expression.Literal(ValueType.IntegerValue(i)))
                .enscope(secID, value)
            case None => ctx.enscope(fm.primID, value)
          }
          val evalCond = fm.cond.map(c => evaluateExpression(c, nctx.push(c))).getOrElse(Expression.Literal(ValueType.BooleanValue(true)))
          evalCond match {
            case Expression.Literal(ValueType.BooleanValue(flag)) => if (flag) {
              val nkey = asAttrKey(fm.keyExpr, nctx).value
              Some(nkey -> evaluateExpression(fm.valExpr, nctx.push(fm.valExpr)))
            } else None
            case _ => nctx.throwError("Filter in for comprehension must evaluate to a boolean")
          }
        }
      case Expression.AbsMapping(values) =>
        values.toList.flatMap { case (key, value) =>
          val nctx = fm.secID match {
            case Some(secID) =>
              ctx.enscope(fm.primID, AttrKey(key))
                .enscope(secID, value)
            case None => ctx.enscope(fm.primID, value)
          }
          val evalCond = fm.cond.map(c => evaluateExpression(c, nctx.push(c))).getOrElse(Expression.Literal(ValueType.BooleanValue(true)))
          evalCond match {
            case Expression.Literal(ValueType.BooleanValue(flag)) => if (flag) {
              val nkey = asAttrKey(fm.keyExpr, nctx).value
              Some(nkey -> evaluateExpression(fm.valExpr, nctx.push(fm.valExpr)))
            } else None
            case _ => nctx.throwError("Filter in for comprehension must evaluate to a boolean")
          }
        }
      case _ => ctx.throwError("Map expression for a for comprehension must evaluate to a map")
    }

    val asMap = if (fm.group) {
      pairs.groupBy(_._1).map(p => (p._1, Expression.AbsSequence(p._2.map(_._2))))
    } else {
      if (pairs.map(_._1).toSet.size != pairs.length) {
        ctx.throwError("More than one entry resolved to the same key")
      } else pairs.toMap
    }

    Expression.AbsMapping(asMap)
  }

  private def asAttrKey(expr: Expression, ctx: Context): Expression.AttrKey = {
    val xctx = ctx.push(expr)
    evaluateExpression(expr, ctx) match {
      case l:Expression.Literal => AttrKey(l.stringify)
      case k:AttrKey => k
      case _: AbsoluteCollection => xctx.throwError("Key expression cannot resolve to a collection")
      case Expression.ResolvedTmpl(value) => AttrKey(value)
    }
  }

  private def evalIndex(target: AbsoluteTerm, index: Expression, ctx: Context): AbsoluteTerm = {
    val ti = evaluateExpression(index, ctx.push(index))

    target match {
      case Expression.AbsSequence(values) => ti match {
        case Expression.Literal(ValueType.IntegerValue(i)) => if (i >= values.length) {
          ctx.throwError(s"Out of bounds access $i for sequence of size ${values.length}")
        } else values(i.toInt)
      }
      case _ => ctx.throwError("The target for an index must be a sequence")
    }
  }
  private def evalAttr(target: AbsoluteTerm, attr: String, ctx: Context): AbsoluteTerm = {
    target match {
      case Expression.AbsMapping(values) => values.get(attr) match {
        case Some(value) => value
        case None => ctx.throwError(s"The object ${values.mkString("{", ", ", "}")} has no key $attr")
      }
      case _ => ctx.throwError("The target for an attribute access must be a map")
    }
  }

  private def splatCommon(target: AbsoluteTerm, ext: List[_], ctx: Context)
                 (f: AbsoluteCollection => Expression)(f2: AbsoluteCollection => Expression): AbsoluteTerm = {
    val tt: Expression.AbsoluteCollection = evaluateExpression(target, ctx.push(target)) match {
      case s: Expression.AbsSequence => s
      case m: Expression.AbsMapping => m
      case o => Expression.AbsSequence(List(o))
    }

    if (ext.isEmpty) {
      tt
    } else if (ext.length == 1) {
      evaluateExpression(f2(tt), ctx)
    } else {
      val comp = f(tt)
      evaluateExpression(comp, ctx)
    }
  }

  private def evalAttrSplat(target: AbsoluteTerm, ext: List[String], ctx: Context): AbsoluteTerm = {
    splatCommon(target, ext, ctx)({ tt: Expression.AbsoluteCollection =>
      val accessChain = ext.tail.dropRight(1).foldLeft(Expression.GetAttr(Expression.Variable("$"), ext.head)) { (stmt, na) =>
        Expression.GetAttr(stmt, na)
      }
      Expression.ForSeqExpr("$", None, tt, accessChain, None)
    })(tt => Expression.GetAttr(tt, ext.head))
  }

  private def evalFullSplat(target: AbsoluteTerm, ext: List[Either[String, Expression]], ctx: Context): AbsoluteTerm = {
    def toAccess(tgt: Expression, a: Either[String, Expression]): Expression.AccessTerm = a match {
      case Right(value) => Expression.Index(tgt, value)
      case Left(value) => Expression.GetAttr(tgt, value)
    }

    splatCommon(target, ext, ctx)({ tt: Expression.AbsoluteCollection =>
      val accessChain = ext.tail.dropRight(1).foldLeft(toAccess(Expression.Variable("$"), ext.head)) { (stmt, na) =>
        toAccess(stmt, na)
      }
      Expression.ForSeqExpr("$", None, tt, accessChain, None)
    })(tt => toAccess(tt, ext.head))
  }

  def keysUsed(set: Set[String], expr: Expression): Set[String] = expr match {
    case Expression.WrappedExpr(e) => keysUsed(set, e)
    case term: Expression.Term => term match {
      case collection: Expression.Collection => collection match {
        case Expression.SequenceT(items) => items.flatMap(e => keysUsed(set, e)).toSet
        case Expression.MappingT(items) => items.flatMap(e => keysUsed(set, e._1) ++ keysUsed(set, e._2)).toSet
      }
      case Expression.Variable(id) => Set(id)
      case Expression.TmplExpr(template) => TmplEval.keysUsed(set, template)
      case Expression.FuncCall(_, args) => args.flatMap(e => keysUsed(set, e)).toSet
      case fexpr: Expression.ForExpr => fexpr match {
        case Expression.ForSeqExpr(a, b, seqExpr, valExpr, cond) =>
          val searchKeys: Set[String] = (set - a) -- b.map(Set(_)).getOrElse(Set.empty[String])
          keysUsed(set, seqExpr) ++ keysUsed(searchKeys, valExpr) ++ cond.map(keysUsed(searchKeys, _)).getOrElse(Set.empty)
        case Expression.ForMapExpr(a, b, seqExpr, keyExpr, valExpr, _, cond) =>
          val searchKeys: Set[String] = (set - a) -- b.map(Set(_)).getOrElse(Set.empty[String])
          keysUsed(set, seqExpr) ++ keysUsed(searchKeys, valExpr) ++ keysUsed(searchKeys, keyExpr) ++ cond.map(keysUsed(searchKeys, _)).getOrElse(Set.empty)
      }
      case aterm: Expression.AccessTerm => aterm match {
        case Expression.Index(target, ind) => keysUsed(set, target) ++ keysUsed(set, ind)
        case Expression.GetAttr(target, _) => keysUsed(set, target)
      }
      case Expression.AttrSplat(target, _) => keysUsed(set, target)
      case Expression.FullSplat(target, ext) => keysUsed(set, target) ++ ext.flatMap {
        case Right(value) => keysUsed(set, value)
        case Left(_) => List.empty
      }.toSet
      case _: AbsoluteTerm => Set.empty
    }
    case operation: Expression.Operation => operation match {
      case Expression.UnaryOp(_, target) => keysUsed(set, target)
      case Expression.BinaryOp(lhs, _, rhs) => keysUsed(set, lhs) ++ keysUsed(set, rhs)
    }
    case Expression.Conditional(cond, yesVal, noVal) =>
      keysUsed(set, cond) ++ keysUsed(set, yesVal) ++ keysUsed(set, noVal)
    case _ => Set.empty
  }
}
