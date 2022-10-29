package dev.jtrim777.hcl4s.parser

import org.parboiled2._

import collection.immutable.Seq
import dev.jtrim777.hcl4s.lang.expr.{ValueType, Expression => expr}
import dev.jtrim777.hcl4s.lang.struct
import dev.jtrim777.hcl4s.lang.expr.operators.{BinaryOperator => bop}

class ParserImpl(val input: ParserInput) extends Parser with SymHelpers with IDHelpers with StringSyntax {

  def DecLiteral: Rule1[String] = rule {
    capture(("-".? ~ CharPredicate.Digit19 ~ zeroOrMore(CharPredicate.Digit)) | '0')
  }
  def HexLiteral: Rule1[String] = rule {
    capture("-".? ~ "0x" ~ oneOrMore(CharPredicate.HexDigit))
  }
  def BinLiteral: Rule1[String] = rule {
    capture("-".? ~ "0b" ~ oneOrMore(ch('0') | ch('1')))
  }

  def StrVal: Rule1[String] = ???

  def IntLiteral: Rule1[expr.Literal] = rule {
    ((HexLiteral | BinLiteral | DecLiteral) ~> helpers.parseNumber _) ~ WSLOp
  }
  def FloatLiteral: Rule1[expr.Literal] = rule {
    (capture("-".? ~ oneOrMore(CharPredicate.Digit) ~ "." ~ oneOrMore(CharPredicate.Digit)) ~> helpers.parseNumber _) ~ WSLOp
  }

  def BoolLit: Rule1[expr.Literal] = rule {
    capture("true" | "false") ~ !(CharPredicate.AlphaNum | '_') ~> helpers.parseBool _
  }

  def NullLit: Rule1[expr.Literal] = rule {
    capture("null") ~ !(CharPredicate.AlphaNum | '_') ~> {_:String => expr.Literal(ValueType.NullValue)}
  }

  def LitVal: Rule1[expr.Literal] = rule {
    IntLiteral | FloatLiteral | BoolLit | NullLit
  }

  def Sequence: Rule1[expr.SequenceT] = rule {
    LB ~ Expression.*(",".wsl) ~ RB ~> {(items:Seq[expr]) => expr.SequenceT(items.toList)}
  }

  def MapElem: Rule1[(expr, expr)] = rule {
    ((ID ~> expr.AttrKey) | Expression) ~ (":" | "=") ~ WSLOp ~ Expression ~> {(e1:expr,e2:expr) => (e1, e2)}
  }
  def Mapping: Rule1[expr.MappingT] = rule {
    LC ~ MapElem.*(",".wsl) ~ RC ~> {(items:Seq[(expr, expr)]) => expr.MappingT(items.toMap)}
  }

  def CollVal: Rule1[expr.Collection] = rule {
    Sequence | Mapping
  }

  def Template: Rule1[expr.TmplExpr] = rule {
    QuotedString ~> {(str:String) => run(new TemplateParser(str).Template)} ~> expr.TmplExpr.apply _
  }

  def Variable: Rule1[expr.Variable] = rule {
    ID ~> expr.Variable
  }

  def FxCall: Rule1[expr.FuncCall] = rule {
    ID ~ LP ~ Expression.*(",".wsl) ~ RP ~> {(id:String, args:Seq[expr]) => expr.FuncCall(id, args.toList)}
  }

  def ForSeqExpr: Rule1[expr.ForSeqExpr] = rule {
    LB ~ "for".keyword ~ ID ~ (",".sym ~ ID).? ~ "in".keywordL ~ Expression ~ ":".symb ~
      Expression ~ ("if".keywordL ~ Expression).? ~ RB ~>
      {(k1:String, k2:Option[String], seq:expr, fx:expr, filt:Option[expr]) =>
        expr.ForSeqExpr(k1, k2, seq, fx, filt)
      }
  }

  def ForMapExpr: Rule1[expr.ForMapExpr] = rule {
    LC ~ "for".keyword ~ ID ~ (",".sym ~ ID).? ~ "in".keywordL ~ Expression ~ ":".symb ~
      Expression ~ "=>".symb ~ Expression ~ capture("...").? ~ ("if".keywordL ~ Expression).? ~ RC ~>
      { (k1: String, k2: Option[String], seq: expr, kfx: expr, vfx: expr, ddd: Option[String], filt: Option[expr]) =>
        expr.ForMapExpr(k1, k2, seq, kfx, vfx, ddd.isDefined, filt)
      }
  }

  def ForExpr: Rule1[expr.ForExpr] = rule {
    ForMapExpr | ForSeqExpr
  }

  def Index: Rule1[expr.Index] = rule {
    Term ~ LB ~ Expression ~ RB ~> {(e1:expr.Term, e2:expr) => expr.Index(e1,e2)}
  }

  def GetAttr: Rule1[expr.GetAttr] = rule {
    Term ~ ".".symb ~ ID ~> {(e1:expr.Term, id:String) => expr.GetAttr(e1,id)}
  }

  def AttrSplat: Rule1[expr.AttrSplat] = rule {
    Term ~ ".*" ~ (".".symb ~ ID).* ~> {(init:expr.Term, ext:Seq[String]) => expr.AttrSplat(init, ext.toList)}
  }

  def SplatExt: Rule1[Either[String, expr]] = rule {
    (".".symb ~ ID ~> {(id:String) => Left(id)}) |
      LB ~ Expression ~ RB ~> {(exp:expr) => Right(exp)}
  }
  def FullSplat: Rule1[expr.FullSplat] = rule {
    Term ~ LB ~ "*" ~ WSOp ~ RB ~ SplatExt.* ~> {(init:expr.Term, exts:Seq[Either[String,expr]]) => expr.FullSplat(init, exts.toList)}
  }

  def Splat: Rule1[expr.Term] = rule {
    AttrSplat | FullSplat
  }

  def Term: Rule1[expr.Term] = rule {
    LitVal | CollVal | Template | Variable | FxCall | ForExpr | Index | GetAttr | Splat |
      (LP ~ Expression ~ RP ~> expr.WrappedExpr)
  }

  def UnaryOp: Rule1[expr.UnaryOp] = rule {
    capture("-" | "!") ~ WSOp ~ Term ~> {(op: String, tgt: expr.Term) => expr.UnaryOp(helpers.parseUnOperator(op), tgt)}
  }

  def BinaryOperator: Rule1[bop[_,_]] = rule {
    capture(
      "==" | "!=" | "<=" | "<" | ">=" | ">" | "+" | "-" |
      "*" | "/" | "%" | "&&" | "||"
    ) ~> helpers.parseBinOperator _
  }
  def BinaryOp: Rule1[expr.BinaryOp] = rule {
    Term ~ BinaryOperator ~ WSLOp ~ Term ~> {(l:expr.Term, o:bop[_,_], r:expr.Term) => expr.BinaryOp(l,o,r)}
  }

  def Operation: Rule1[expr] = rule {
    UnaryOp | BinaryOp
  }

  def Conditional: Rule1[expr] = rule {
    Term ~ "?".sym ~ Term ~ ":".sym ~ Term ~> {(e1:expr.Term, e2:expr.Term, e3:expr.Term) => expr.Conditional(e1,e2,e3)}
  }

  def Expression: Rule1[expr] = rule {
    Term | Operation | Conditional
  }

  def Attribute: Rule1[struct.Attribute] = rule {
    ID ~ "=".symb ~ Expression ~ NLReq ~> {(id:String, exp: expr) => struct.BodyElemT.AttributeT(id, exp)}
  }

  def OLBlock: Rule1[struct.Block] = rule {
    ID ~ (ID | StrVal).+ ~ "{".sym ~
      (ID ~ "=".symb ~ Expression ~> {(name:String, value:expr) => struct.BodyElemT.AttributeT(name, value)}).? ~
      "}".sym ~ NLReq ~>
      { (kind: String, labels: Seq[String], elems: Option[struct.BodyElem]) => struct.BodyElemT.BlockT(kind, labels.toList, elems.toList) }
  }
  def Block: Rule1[struct.Block] = rule {
    ID ~ (ID | StrVal).* ~ LC ~ BodyElem.* ~ RC ~ NLReq ~>
      {(kind:String, labels:Seq[String], elems:Seq[struct.BodyElem]) => struct.BodyElemT.BlockT(kind, labels.toList, elems.toList)}
  }

  def BodyElem: Rule1[struct.BodyElem] = rule {
    Attribute | Block | OLBlock
  }

  def HCL: Rule1[struct.HCLSource] = rule {
    BodyElem.* ~> {(elems:Seq[struct.BodyElem]) => struct.HCLSourceT(elems.toList)}
  }
}
