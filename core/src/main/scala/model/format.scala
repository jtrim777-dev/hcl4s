package dev.jtrim777.hcl4s.model

import org.apache.commons.text.StringEscapeUtils

object format {
  def format(model: HCLBody, indent: Int = 4, compact: Boolean = false): String = {
    val attrsSorted = model.attributes.toList.sortBy(_._1).map(t => writeAttribute(t._1, t._2, indent, compact))

    val blocksSorted = model.blocks.sortBy(_.key).map(writeBlock(_, indent, compact))

    val itemBreak = if (compact) "\n" else "\n\n"
    val segmentBreak = if (compact) {
      "\n"
    } else if (attrsSorted.isEmpty) {
      ""
    } else if (blocksSorted.isEmpty) {
      "\n"
    } else "\n\n\n"

    attrsSorted.mkString("", itemBreak, segmentBreak) + blocksSorted.mkString("", itemBreak, "\n")
  }

  private def shiftText(text: String, indent: Int): String = {
    val tab = " " * indent
    text.split('\n').mkString(tab, "\n"+tab, "")
  }

  private[model] def writeBlock(block: HCLBlock, indent: Int, compact: Boolean): String = {
    val wrappedLabels = block.labels.map(s => "\"" + s + "\"").mkString(" ", " ", "")
    val header = block.kind + wrappedLabels + " {\n"
    val footer = "}"

    val content = shiftText(format(block.body, indent = indent, compact = compact), indent) + "\n"

    header + content + footer
  }
  private[model] def writeAttribute(key: String, value: HCLValue, indent: Int, compact: Boolean): String = {
    key + " = " + writeValue(value, indent, compact)
  }

  private[model] def writeValue(value: HCLValue, indent: Int, compact: Boolean): String = value match {
    case HCLValue.Number(value) => if (value.toLong == value) value.toLong.toString else value.toString
    case HCLValue.Bool(value) => value.toString
    case HCLValue.Text(value) => "\"" + StringEscapeUtils.escapeJava(value) + "\""
    case HCLValue.Null => "null"
    case l: HCLValue.HCLList => writeSequence(l, indent, compact)
    case o: HCLValue.HCLObject => writeObject(o, indent, compact)
    case HCLValue.BlockReference(path) => path.mkString(".")
  }

  private[model] def writeSequence(value: HCLValue.HCLList, indent: Int, compact: Boolean): String = {
    val oneLine = compact || value.values.length < 2
    val items = value.values.map(writeValue(_, indent, compact))

    if (oneLine) {
      val bracePad = if (compact) "" else " "
      val sep = if (compact) "," else ", "
      val content = shiftText(items.mkString(sep), indent)
      "["+bracePad + content + bracePad+"]"
    } else {
      val content = shiftText(items.mkString(",\n"), indent)
      "[\n" + content + "\n]"
    }
  }

  private[model] def writeObject(value: HCLValue.HCLObject, indent: Int, compact: Boolean): String = {
    val items = value.data.toList.sortBy(_._1)
    val oneLine = compact || value.data.size < 2
    val kvSep = if (compact) ":" else " = "
    val entries = items.map(t => t._1 + kvSep + writeValue(t._2, indent, compact))

    if (oneLine) {
      val bracePad = if (compact) "" else " "
      val sep = if (compact) "," else ", "
      val content = shiftText(entries.mkString(sep), indent)
      "{" + bracePad + content + bracePad + "}"
    } else {
      val content = shiftText(entries.mkString(",\n"), indent)
      "{\n" + content + "\n}"
    }
  }
}
