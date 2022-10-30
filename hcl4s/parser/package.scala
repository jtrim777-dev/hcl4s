package dev.jtrim777.hcl4s

import dev.jtrim777.hcl4s.lang.struct.HCLSource

import java.nio.file.{Files, Path}
import fastparse.{Parsed, parse => libparse}

package object parser {
  def parse(source: String): HCLSource = {
    libparse(source, Parser.HCL(_)) match {
      case Parsed.Success(value, _) => value
      case failure: Parsed.Failure => throw new IllegalArgumentException(failure.longMsg)
    }
  }

  def load(path: Path): HCLSource = parse(Files.readString(path))
}
