package dev.jtrim777
package hcl4s.parser

import org.parboiled2._

trait SymHelpers extends Parser with WSHelpers {
  def LP: Rule0 = rule {
    "(".wsl
  }

  def RP: Rule0 = rule {
    ")".wsl
  }

  def LB: Rule0 = rule {
    "[".wsl
  }

  def RB: Rule0 = rule {
    "]".wsl
  }

  def LC: Rule0 = rule {
    "{".wsl
  }

  def RC: Rule0 = rule {
    "}".wsl
  }
}
