package dev.jtrim777.hcl4s.model

import scala.language.dynamics

sealed trait HCLValue extends Dynamic {
  val kind: String

  def apply(i: Int): HCLValue = {
    this match {
      case HCLValue.HCLList(values) => values(i)
      case _ => throw new NoSuchElementException(s"HCLValue of type '$kind' cannot be indexed")
    }
  }

  def selectDynamic(name: String): HCLValue = {
    this match {
      case HCLValue.HCLObject(data) => data(name)
      case _ => throw new NoSuchElementException(s"HCLValue of type '$kind' cannot be field-accessed")
    }
  }
}

object HCLValue {
  case class Number(value: Double) extends HCLValue {
    override val kind: String = "number"
  }
  case class Bool(value: Boolean) extends HCLValue {
    override val kind: String = "boolean"
  }
  case class Text(value: String) extends HCLValue {
    override val kind: String = "string"
  }
  case object Null extends HCLValue {
    override val kind: String = "null"
  }

  case class HCLList(values: List[HCLValue]) extends HCLValue {
    override val kind: String = "list"
  }
  case class HCLObject(data: Map[String, HCLValue]) extends HCLValue {
    override val kind: String = "object"

    def put(key: String, value: HCLValue): HCLObject = {
      if (data.contains(key)) {
        data(key) match {
          case HCLList(values) => this.copy(data = data.updated(key, HCLList(values :+ value)))
          case o => this.copy(data = data.updated(key, HCLList(List(o, value))))
        }
      } else {
        this.copy(data = data.updated(key, value))
      }
    }

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

  case class BlockReference(path: List[String]) extends HCLValue {
    override val kind: String = "ref"
  }
}
