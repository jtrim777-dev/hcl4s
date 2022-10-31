package dev.jtrim777.hcl4s.parser

import dev.jtrim777.hcl4s.lang.expr.{operators => ops}
import dev.jtrim777.hcl4s.lang.expr.operators.{BinaryOperator => bop}
import dev.jtrim777.hcl4s.lang.expr.{ValueType, Expression => expr}
import dev.jtrim777.hcl4s.lang.struct
import fastparse.internal.Logger

import scala.collection.mutable

//import org.parboiled2._
import fastparse._, JavaWhitespace._
import FastparseUtils._, IDHelpers._

private[parser] object Parser {
  var doLog = false
//  val log: mutable.Buffer[String] = collection.mutable.Buffer.empty[String]
  implicit val logger: Logger = Logger(inp => if (doLog) println(inp) else ())
//  implicit val logger: Logger = Logger(_ => ())
//  implicit val logger: Logger = Logger(log.append)

  def DecLiteral[_: P]: Rule1[String] = P {
    (("-".? ~~ CharDigit19 ~~ CharDigit.repX) | "0").!
  }.log
  def HexLiteral[_: P]: Rule1[String] = P {
    ("-".? ~~ "0x" ~~ CharHexDigit.repX(1)).!
  }.log
  def BinLiteral[_: P]: Rule1[String] = P {
    ("-".? ~~ "0b" ~~ CharIn("01").repX(1)).!
  }.log

  def IntLiteral[_: P]: Rule1[expr.Literal] = P {
    (HexLiteral | BinLiteral | DecLiteral) ~> helpers.parseNumber
  }.log
  def FloatLiteral[_: P]: Rule1[expr.Literal] = P {
    ("-".? ~~ CharDigit.repX(1) ~~ "." ~~ CharDigit.repX(1)).! ~> helpers.parseNumber
  }.log

  def BoolLit[_: P]: Rule1[expr.Literal] = P {
    ("true" | "false").! ~~ !CharIn("A-Za-z0-9_") ~> helpers.parseBool
  }.log

  def NullLit[_: P]: Rule1[expr.Literal] = P {
    "null" ~~ !CharIn("A-Za-z0-9_") ~> expr.Literal(ValueType.NullValue)
  }.log

  def LitVal[_: P]: Rule1[expr.Literal] = P {
    FloatLiteral | IntLiteral | BoolLit | NullLit
  }.log

  def Sequence[_: P]: Rule1[expr.SequenceT] = P {
    "[" ~ Expression.rep(sep=","./) ~ "]" ~> {items => expr.SequenceT(items.toList)}
  }.log

  def MapElem[_ : P]: Rule1[(expr, expr)] = P {
    ((ID ~> expr.AttrKey) | Expression) ~ (":" | "=") ~ Expression
  }.log
  def Mapping[_: P]: Rule1[expr.MappingT] = P {
    "{" ~ MapElem.rep(sep=","./) ~ "}" ~> {items => expr.MappingT(items.toMap)}
  }.log

  def CollVal[_: P]: Rule1[expr.Collection] = P {
    Sequence | Mapping
  }.log

  def Template[_: P]: Rule1[expr.TmplExpr] = P {
    StringSyntax.QuotedString ~> {(str:String) =>
      val result = parse(str, TemplateParser.Template(_))
      expr.TmplExpr(result.get.value)
    }
  }.log

  def Variable[_: P]: Rule1[expr.Variable] = P {
    ID ~> expr.Variable
  }.log

  def FxCall[_: P]: Rule1[expr.FuncCall] = P {
    ID ~~ "(" ~ Expression.rep(sep=","./) ~ ")" ~> {tuple => expr.FuncCall(tuple._1, tuple._2.toList)}
  }.log

  def ForSeqExpr[_: P]: Rule1[expr.ForSeqExpr] = P {
    "[" ~ "for" ~ ID ~ ("," ~ ID).? ~ "in" ~ Expression ~ ":" ~
      Expression ~ ("if" ~ Expression).? ~ "]" ~> expr.ForSeqExpr.tupled
  }.log

  def ForMapExpr[_: P]: Rule1[expr.ForMapExpr] = P {
    "{" ~ "for" ~ ID ~ ("," ~ ID).? ~ "in" ~ Expression ~ ":" ~
      Expression ~ "=>" ~ Expression ~ ("...".!.? ~> {o => o.isDefined}) ~
      ("if" ~ Expression).? ~ "}" ~> expr.ForMapExpr.tupled
  }.log

  def ForExpr[_: P]: Rule1[expr.ForExpr] = P {
    ForMapExpr | ForSeqExpr
  }.log

  def Index[_: P]: Rule1[ops.IndexOp] = P {
    "[" ~/ Expression ~ "]" ~> ops.IndexOp
  }.log

  def GetAttr[_: P]: Rule1[ops.AccessOp] = P {
    "." ~/ ID ~> ops.AccessOp
  }.log

  def AttrSplat[_: P]: Rule1[ops.ASplatOp.type] = P {
    P(".*") ~> ops.ASplatOp
  }.log

  def FullSplat[_: P]: Rule1[ops.FSplatOp.type ] = P {
    "[" ~ "*" ~/ "]" ~> ops.FSplatOp
  }.log

  def PFOp[_: P]: Rule1[ops.PostfixOp] = P {
    AttrSplat | FullSplat | Index | GetAttr
  }.log

  def TermCore[_: P]: Rule1[expr.Term] = P {
    LitVal | CollVal | Template | Variable | FxCall | ForExpr | ("(" ~/ Expression ~ ")" ~> expr.WrappedExpr)
  }.log

  def Term[_: P]: Rule1[expr.Term] = P {
    TermCore ~ PFOp.repX ~> {data => helpers.combineTerm(data._1, data._2)}
  }.log


  def UnaryOp[_: P]: Rule1[expr.UnaryOp] = P {
    ("-" | "!").! ~ Term ~> {case (op: String, tgt: expr.Term) => expr.UnaryOp(helpers.parseUnOperator(op), tgt)}
  }.log

  def BinaryOperator[_: P]: Rule1[bop[_,_]] = P {
    (
      "==" | "!=" | "<=" | "<" | ">=" | ">" | "+" | "-" |
      "*" | "/" | "%" | "&&" | "||"
    ).! ~> helpers.parseBinOperator
  }.log
  def BinaryOp[_: P]: Rule1[expr.BinaryOp] = P {
    NoCut(Term ~ BinaryOperator) ~/ Term ~> {case (l:expr.Term, o:bop[_,_], r:expr.Term) => expr.BinaryOp(l,o,r)}
  }.log

  def Operation[_: P]: Rule1[expr] = P {
    UnaryOp | BinaryOp
  }.log

  def Conditional[_: P]: Rule1[expr] = P {
    NoCut(Term ~ "?") ~/ Term ~ ":" ~/ Term ~> {case (e1:expr.Term, e2:expr.Term, e3:expr.Term) => expr.Conditional(e1,e2,e3)}
  }.log

  def Expression[_: P]: Rule1[expr] = P {
    Operation | Conditional | Term
  }.log

  def Attribute[_: P]: Rule1[struct.Attribute] = P {
    ID ~ "=" ~ Expression  ~> {case (id:String, exp: expr) => struct.BodyElemT.AttributeT(id, exp)}
  }.log

//  def OLBlock[_: P]: Rule1[struct.Block] = P {
//    ID ~ (ID | StringSyntax.QuotedString).rep(1) ~ "{" ~
//      (ID ~ "=" ~ Expression ~> {case (name:String, value:expr) => struct.BodyElemT.AttributeT(name, value)}).? ~
//      "}"  ~>
//      { case (kind: String, labels: Seq[String], elems: Option[struct.BodyElem]) =>
//        struct.BodyElemT.BlockT(kind, labels.toList, elems.toList)
//      }
//  }
  def Block[_: P]: Rule1[struct.Block] = P {
    ID ~ (ID | StringSyntax.QuotedString).rep ~ "{" ~ BodyElem.rep(sep=""./) ~ "}"  ~>
      { case (kind:String, labels:Seq[String], elems:Seq[struct.BodyElem]) =>
        struct.BodyElemT.BlockT(kind, labels.toList, elems.toList)
      }
  }.log

  def BodyElem[_: P]: Rule1[struct.BodyElem] = P {
    Attribute | Block //| OLBlock
  }.log

  def HCL[_: P]: Rule1[struct.HCLSource] = P {
    CharsWhileIn(" \t\n\r", 0) ~ BodyElem.rep ~ End ~> {elems => struct.HCLSourceT(elems.toList)}
  }.log
}
