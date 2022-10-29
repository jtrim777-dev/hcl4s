package dev.jtrim777.hcl4s.parser

import fastparse._, NoWhitespace._
import FastparseUtils._

object StringSyntax {
  private[StringSyntax] def QuoteBackslash[_: P]: Rule0 = CharIn("\"\\\\")
  private[StringSyntax] def QuoteSlashBackSlash[_: P]: Rule0 = CharIn("\"\\\\/")

  def QuotedString[_: P]: Rule1[String] = P {
    "\"" ~/ Characters ~ "\"" ~> {chars => chars.mkString("")}
  }

  def Characters[_: P]: Rule1[Seq[String]] = P {
    (NormalChar.! | ("\\" ~ EscapedChar)).*
  }

  def NormalChar[_: P]: Rule0 = P {
    !QuoteBackslash ~ AnyChar
  }

  def EscapedChar[_: P]: Rule1[String] = P {
    QuoteSlashBackSlash.! |
      P("b") ~> "\b" |
      P("f") ~> "\f" |
      P("n") ~> "\n" |
      P("r") ~> "\r" |
      P("t") ~> "\t" |
      Unicode ~> { (code:Int) => code.asInstanceOf[Char].toString }
  }

  def Unicode[_: P]: Rule1[Int] = P {
    "u" ~ (CharHexDigit ~ CharHexDigit ~ CharHexDigit ~ CharHexDigit).! ~> {(s:String) => java.lang.Integer.parseInt(s, 16)}
  }
}