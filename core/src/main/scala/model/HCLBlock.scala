package dev.jtrim777.hcl4s.model

case class HCLBlock(kind: String, labels: List[String], body: HCLBody) {
  def key: String = kind + (if (labels.isEmpty) "" else labels.mkString(".", ".", ""))
}
