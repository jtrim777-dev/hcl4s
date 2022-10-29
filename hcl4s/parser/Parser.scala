package dev.jtrim777.hcl4s.parser

import dev.jtrim777.hcl4s.lang.expr.operators.{BinaryOperator => bop}
import dev.jtrim777.hcl4s.lang.expr.{ValueType, Expression => expr}
import dev.jtrim777.hcl4s.lang.struct

//import org.parboiled2._
import fastparse._, JavaWhitespace._
import FastparseUtils._, IDHelpers._

import scala.collection.immutable.Seq

object Parser {

  def DecLiteral[_: P]: Rule1[String] = P {
    (("-".? ~~ CharDigit19 ~~ CharDigit.repX) | "0").!
  }
  def HexLiteral[_: P]: Rule1[String] = P {
    ("-".? ~~ "0x" ~~ CharHexDigit.repX(1)).!
  }
  def BinLiteral[_: P]: Rule1[String] = P {
    ("-".? ~~ "0b" ~~ CharIn("01").repX(1)).!
  }

  def StrVal: Rule1[String] = ???

  def IntLiteral[_: P]: Rule1[expr.Literal] = P {
    (HexLiteral | BinLiteral | DecLiteral) ~> helpers.parseNumber
  }
  def FloatLiteral[_: P]: Rule1[expr.Literal] = P {
    ("-".? ~~ CharDigit.repX(1) ~~ "." ~~ CharDigit.repX(1)).! ~> helpers.parseNumber
  }

  def BoolLit[_: P]: Rule1[expr.Literal] = P {
    ("true" | "false").! ~ !CharIn("A-Za-z0-9_") ~> helpers.parseBool
  }

  def NullLit[_: P]: Rule1[expr.Literal] = P {
    "null" ~ !CharIn("A-Za-z0-9_") ~> expr.Literal(ValueType.NullValue)
  }

  def LitVal[_: P]: Rule1[expr.Literal] = P {
    IntLiteral | FloatLiteral | BoolLit | NullLit
  }

  def Sequence[_: P]: Rule1[expr.SequenceT] = P {
    "[" ~ Expression.*(",") ~ "]" ~> {items => expr.SequenceT(items.toList)}
  }

  def MapElem[_ : P]: Rule1[(expr, expr)] = P {
    ((ID ~> expr.AttrKey) | Expression) ~ (":" | "=") ~ Expression
  }
  def Mapping[_: P]: Rule1[expr.MappingT] = P {
    "{" ~ MapElem.*(",") ~ "}" ~> {items => expr.MappingT(items.toMap)}
  }

  def CollVal[_: P]: Rule1[expr.Collection] = P {
    Sequence | Mapping
  }

  def Template[_: P]: Rule1[expr.TmplExpr] = P {
    StringSyntax.QuotedString ~> {(str:String) =>
      val result = parse(str, TemplateParser.Template(_))
      expr.TmplExpr(result.get.value)
    }
  }

  def Variable[_: P]: Rule1[expr.Variable] = P {
    ID ~> expr.Variable
  }

  def FxCall[_: P]: Rule1[expr.FuncCall] = P {
    ID ~~ "(" ~ Expression.*(",") ~ ")" ~> {tuple => expr.FuncCall(tuple._1, tuple._2.toList)}
  }

  def ForSeqExpr[_: P]: Rule1[expr.ForSeqExpr] = P {
    "[" ~ "for" ~ ID ~ ("," ~ ID).? ~ "in" ~ Expression ~ ":" ~
      Expression ~ ("if" ~ Expression).? ~ "]" ~> expr.ForSeqExpr.tupled
  }

  def ForMapExpr[_: P]: Rule1[expr.ForMapExpr] = P {
    "{" ~ "for" ~ ID ~ ("," ~ ID).? ~ "in" ~ Expression ~ ":" ~
      Expression ~ "=>" ~ Expression ~ ("...".!.? ~> {o => o.isDefined}) ~
      ("if" ~ Expression).? ~ "}" ~> expr.ForMapExpr.tupled
  }

  def ForExpr[_: P]: Rule1[expr.ForExpr] = P {
    ForMapExpr | ForSeqExpr
  }

  def Index[_: P]: Rule1[expr.Index] = P {
    Term ~ "[" ~ Expression ~ "]" ~> expr.Index.tupled
  }

  def GetAttr[_: P]: Rule1[expr.GetAttr] = P {
    Term ~ "." ~ ID ~> expr.GetAttr.tupled
  }

  def AttrSplat[_: P]: Rule1[expr.AttrSplat] = P {
    Term ~ ".*" ~ ("." ~ ID).* ~> {tuple => expr.AttrSplat(tuple._1, tuple._2.toList)}
  }

  def SplatExt[_: P]: Rule1[Either[String, expr]] = P {
    ("." ~ ID ~> {(id:String) => Left(id)}) | "[" ~ Expression ~ "]" ~> {(exp:expr) => Right(exp)}
  }
  def FullSplat[_: P]: Rule1[expr.FullSplat] = P {
    Term ~ "[" ~ "*" ~ "]" ~ SplatExt.* ~> {tuple => expr.FullSplat(tuple._1, tuple._2.toList)}
  }

  def Splat[_: P]: Rule1[expr.Term] = P {
    AttrSplat | FullSplat
  }

  def Term[_: P]: Rule1[expr.Term] = P {
    LitVal | CollVal | Template | Variable | FxCall | ForExpr | Index | GetAttr | Splat |
      ("(" ~ Expression ~ ")" ~> expr.WrappedExpr)
  }

  def UnaryOp[_: P]: Rule1[expr.UnaryOp] = P {
    ("-" | "!").! ~ Term ~> {case (op: String, tgt: expr.Term) => expr.UnaryOp(helpers.parseUnOperator(op), tgt)}
  }

  def BinaryOperator[_: P]: Rule1[bop[_,_]] = P {
    (
      "==" | "!=" | "<=" | "<" | ">=" | ">" | "+" | "-" |
      "*" | "/" | "%" | "&&" | "||"
    ).! ~> helpers.parseBinOperator
  }
  def BinaryOp[_: P]: Rule1[expr.BinaryOp] = P {
    Term ~ BinaryOperator ~ Term ~> {case (l:expr.Term, o:bop[_,_], r:expr.Term) => expr.BinaryOp(l,o,r)}
  }

  def Operation[_: P]: Rule1[expr] = P {
    UnaryOp | BinaryOp
  }

  def Conditional[_: P]: Rule1[expr] = P {
    Term ~ "?" ~ Term ~ ":" ~ Term ~> {case (e1:expr.Term, e2:expr.Term, e3:expr.Term) => expr.Conditional(e1,e2,e3)}
  }

  def Expression[_: P]: Rule1[expr] = P {
    Term | Operation | Conditional
  }

  def Attribute[_: P]: Rule1[struct.Attribute] = P {
    ID ~ "=" ~ Expression ~ "\n" ~> {case (id:String, exp: expr) => struct.BodyElemT.AttributeT(id, exp)}
  }

  def OLBlock[_: P]: Rule1[struct.Block] = P {
    ID ~ (ID | StrVal).+ ~ "{" ~
      (ID ~ "=" ~ Expression ~> {case (name:String, value:expr) => struct.BodyElemT.AttributeT(name, value)}).? ~
      "}" ~ "\n" ~>
      { case (kind: String, labels: Seq[String], elems: Option[struct.BodyElem]) =>
        struct.BodyElemT.BlockT(kind, labels.toList, elems.toList)
      }
  }
  def Block[_: P]: Rule1[struct.Block] = P {
    ID ~ (ID | StrVal).* ~ "{" ~ BodyElem.* ~ "}" ~ "\n" ~>
      { case (kind:String, labels:Seq[String], elems:Seq[struct.BodyElem]) =>
        struct.BodyElemT.BlockT(kind, labels.toList, elems.toList)
      }
  }

  def BodyElem[_: P]: Rule1[struct.BodyElem] = P {
    Attribute | Block | OLBlock
  }

  def HCL[_: P]: Rule1[struct.HCLSource] = P {
    BodyElem.* ~> {elems => struct.HCLSourceT(elems.toList)}
  }
}
