package dev.jtrim777.hcl4s.model

import scala.annotation.tailrec

case class HCLBody(attributes: Map[String, HCLValue], blocks: List[HCLBlock]) {
  def toObject: HCLValue.HCLObject = {
    val base = HCLValue.HCLObject.Empty

    val afterAttrs = attributes.foldLeft(base) { case (obj, (k, v)) => obj.put(k, v) }

    blocks.foldLeft(afterAttrs) { (obj, block) =>
      val kh = prepareKey(block.kind)
      val path = kh :: block.labels
      obj.put(path, block.body.toObject)
    }
  }

  @tailrec
  private def prepareKey(k: String): String = if (attributes.contains(k)) prepareKey(k+"$") else k
}
