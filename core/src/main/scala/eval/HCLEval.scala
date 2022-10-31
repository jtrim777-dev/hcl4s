package dev.jtrim777.hcl4s.eval

import dev.jtrim777.hcl4s.lang.expr.Expression.{AbsoluteTerm, AttrKey}
import dev.jtrim777.hcl4s.lang.expr.{Expression, ValueType}
import dev.jtrim777.hcl4s.lang.struct.BodyElemT.{AttributeT, BlockT}
import dev.jtrim777.hcl4s.lang.struct._
import dev.jtrim777.hcl4s.model.{HCLBlock, HCLBody, HCLValue}
import dev.jtrim777.hcl4s.util.Trace

object HCLEval {
  def evaluate(source: HCLSource, externalVariables: Map[String, HCLValue] = Map.empty,
               functions: Map[String, HCLFunction] = Map.empty, scopeBlockTags: Boolean = true): HCLBody = {
    val output = innerEvaluate(source, externalVariables.map(p => (p._1, valueToTerm(p._2))), functions, scopeBlockTags)

    convertBlock(output).body
  }

  private def convertBlock(block: ResolvedBlock): HCLBlock = {
    val blocks = block.content.collect {
      case b: ResolvedBlock => b
    }.map(convertBlock)

    val attrs = block.content.collect {
      case a: ResolvedAttribute => a
    }.map(ra => ra.name -> termToValue(ra.value))

    HCLBlock(block.kind, block.labels, HCLBody(attrs.toMap, blocks))
  }

  private def valueToTerm(value: HCLValue): AbsoluteTerm = value match {
    case HCLValue.Number(value) => Expression.Literal(ValueType.FloatingValue(value))
    case HCLValue.Bool(value) => Expression.Literal(ValueType.BooleanValue(value))
    case HCLValue.Text(value) => Expression.ResolvedTmpl(value)
    case HCLValue.Null => Expression.Literal(ValueType.NullValue)
    case HCLValue.HCLList(values) => Expression.AbsSequence(values.map(valueToTerm))
    case HCLValue.HCLObject(data) => Expression.AbsMapping(data.map { case (k,v) =>
      (k, valueToTerm(v))
    })
  }

  private def termToValue(term: AbsoluteTerm): HCLValue = term match {
    case Expression.Literal(value) => value match {
      case num: ValueType.NumericValue[_] => HCLValue.Number(num.asDouble)
      case ValueType.BooleanValue(b) => HCLValue.Bool(b)
      case ValueType.NullValue => HCLValue.Null
    }
    case Expression.AttrKey(key) => HCLValue.Text(key)
    case coll: Expression.AbsoluteCollection => coll match {
      case Expression.AbsSequence(items) => HCLValue.HCLList(items.map(termToValue))
      case Expression.AbsMapping(items) => HCLValue.HCLObject(items.map { case (k, v) =>
        (k, termToValue(v))
      })
    }
    case Expression.ResolvedTmpl(text) => HCLValue.Text(text)
  }

  private def innerEvaluate(hcl: HCLSource, scope: Map[String, AbsoluteTerm],
               functions: Map[String, HCLFunction], scopeBlockTags: Boolean): ResolvedBlock = {
    val rootBlock = BlockT(".", List.empty, hcl.elements)
    val ctx = Context(ScopeStack(scope), Trace.empty, functions, scopeBlockTags)

    evaluateBlock(rootBlock, ctx)
  }

  private def evaluateAttribute(attr: Attribute, ctx: Context): ResolvedAttribute = {
    AttributeT(attr.name, ExprEval.evaluateExpression(attr.value, ctx.push(attr.value)))
  }

  private def evaluateBlock(block: Block, ctx: Context): ResolvedBlock = {
    val namedElems = scanElements(block.content, ctx)
    val keys = namedElems.flatMap(_._1).toSet

    if (keys.size != namedElems.flatMap(_._1).length) {
      namedElems.foldLeft(List.empty[String]) { case (known, (k, v)) =>
        k match {
          case Some(value) => if (known.contains(value)) {
            ctx.throwError(s"Illegal redefinition for identifier '$value'") // TODO: Scope to value
          } else value :: known
          case None => known
        }
      }
    }

    val deps = namedElems.foldLeft(Map.empty[String, Set[String]]) { case (ideps, (oname, elem)) =>
      oname.map(name => ideps.updated(name, keysUsed(keys, elem, ctx))).getOrElse(ideps)
    }

    def depsOf(key: String, found: Set[String] = Set.empty): Set[String] = {
      if (deps.contains(key)) {
        val newDeps = deps(key) -- found
        val allDeps = deps(key).union(found)

        val result = newDeps.flatMap(depsOf(_, allDeps))

        if (result.contains(key)) {
          ctx.throwError("Mutual recursion found in resolution dependencies") // TODO: Scope to value
        } else result
      } else found
    }

    val ordered = namedElems.sortWith { case ((name1, _), (name2, _)) =>
      name1 match {
        case Some(name1t) => name2 match {
          case Some(name2t) => depsOf(name2t).contains(name1t)
          case None => true
        }
        case None => false
      }
    }

    val (_, rez) = ordered.foldLeft((ctx, List.empty[ResolvedBody])) { case ((ictx, accum), (name, elem)) =>
      val (resolved, nctx): (ResolvedBody, Context) = elem match {
        case a:Attribute =>
          val rez = evaluateAttribute(a, ictx.push(a))
          val ctx = name.map(ictx.enscope(_, elemToValue(rez, ictx))).getOrElse(ictx)
          (rez, ctx)
        case b:Block =>
          val rez = evaluateBlock(b, ictx.push(b))
          val key = b.kind :: b.labels
          val ctx = name.map(_ => ictx.enscope(key, elemToValue(rez, ictx))).getOrElse(ictx)
          (rez, ctx)
      }
      val naccum = resolved :: accum
      (nctx, naccum)
    }

    BlockT(block.kind, block.labels, rez)
  }

  // Collect all named elements at this level of a block
  private def scanElements[T <: Expression](elements: List[BodyElemT[T]], ctx: Context): List[(Option[String], BodyElemT[T])] = {
    elements.map {
      case a: Attribute => Some(a.name) -> a
      case b: Block => if (ctx.scopeBlockTags) {
        val k = b.kind + (if (b.labels.nonEmpty) b.labels.mkString(".", ".", "") else "")
        Some(k) -> b
      } else None -> b
    }
  }

  // Determine if the element used the given symbol
  private def keysUsed(set: Set[String], element: BodyElem, ctx: Context): Set[String] = element match {
    case a:Attribute => ExprEval.keysUsed(set, a.value)
    case b:Block =>
      val namedElems = scanElements(b.content, ctx)
      val allKeys = namedElems.flatMap(_._1).toSet
      namedElems.foldLeft(Set.empty[String]) { case (nset, (ikey, elem)) =>
        val searchSet = if (ikey.isEmpty) set -- allKeys else (set -- allKeys) + ikey.get
        nset.union(keysUsed(searchSet, elem, ctx))
      }
  }

  private def elemToValue(elem: ResolvedBody, ctx: Context): AbsoluteTerm = elem match {
    case a:ResolvedAttribute => a.value
    case b:ResolvedBlock =>
      val mapi = scanElements[AbsoluteTerm](b.content, ctx).foldLeft(Map.empty[String, AbsoluteTerm]) { case (accum, (nameo, value)) =>
        nameo.map(s => accum.updated(s, elemToValue(value, ctx))).getOrElse(accum)
      }
      Expression.AbsMapping(mapi)
  }
}
