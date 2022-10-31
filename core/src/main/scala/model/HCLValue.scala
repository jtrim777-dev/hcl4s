package dev.jtrim777.hcl4s.model

sealed trait HCLValue {

}

object HCLValue {
  case class Number(value: Double) extends HCLValue
  case class Bool(value: Boolean) extends HCLValue
  case class Text(value: String) extends HCLValue
  case object Null extends HCLValue

  case class HCLList(values: List[HCLValue]) extends HCLValue
  case class HCLObject(data: Map[String, HCLValue]) extends HCLValue {
    def put(key: String, value: HCLValue): HCLObject = this.copy(data = data.updated(key, value))

    def put(path: List[String], value: HCLValue): HCLObject = {
      if (path.isEmpty) {
        throw new IllegalArgumentException("Cannot insert value with empty path")
      } else if (path.length == 1) {
        this.put(path.head, value)
      } else {
        data.get(path.head) match {
          case Some(obj: HCLObject) => this.copy(data = data.updated(path.head, obj.put(path.tail, value)))
          case _ => this.copy(data = data.updated(path.head, HCLObject.Empty.put(path.tail, value)))
        }
      }
    }
  }
  object HCLObject {
    val Empty: HCLObject = HCLObject(Map.empty)
  }

  case class BlockReference(path: List[String]) extends HCLValue
}
