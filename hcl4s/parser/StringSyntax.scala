package dev.jtrim777.hcl4s.parser

import org.parboiled2.CharPredicate.HexDigit
import org.parboiled2._

trait StringSyntax extends Parser with WSHelpers with StringBuilding {
  import StringSyntax._

  def QuotedString: Rule1[String] = rule {
    '"' ~ clearSB() ~ Characters ~ "\"".wsl ~ push(sb.toString)
  }

  def Characters: Rule0 = rule(zeroOrMore(NormalChar | '\\' ~ EscapedChar))

  def NormalChar: Rule0 = rule(!QuoteBackslash ~ ANY ~ appendSB())

  def EscapedChar: Rule0 = rule {
    QuoteSlashBackSlash ~ appendSB() |
      'b' ~ appendSB('\b') |
      'f' ~ appendSB('\f') |
      'n' ~ appendSB('\n') |
      'r' ~ appendSB('\r') |
      't' ~ appendSB('\t') |
      Unicode ~> { (code:Int) => sb.append(code.asInstanceOf[Char]); () }
  }

  def Unicode: Rule1[Int] = rule{
    'u' ~ capture(HexDigit ~ HexDigit ~ HexDigit ~ HexDigit) ~> {(s:String) => java.lang.Integer.parseInt(s, 16)}
  }
}

object StringSyntax {
  private[StringSyntax] val QuoteBackslash = CharPredicate("\"\\")
  private[StringSyntax] val QuoteSlashBackSlash = QuoteBackslash ++ "/"
}