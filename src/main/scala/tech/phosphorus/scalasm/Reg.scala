package tech.phosphorus.scalasm

class Reg(val name: String, val alias: String, val regType: RegType) {

  def updateAlias(alias: String): Reg = Reg(name, alias, regType)

  override def toString = s"Reg(name=$name, alias=$alias, regType=$regType)"

}

object Reg {

  def apply(name: String, alias: String, regType: RegType): Reg = new Reg(name, alias, regType)

  def apply(name: String, regType: RegType): Reg = new Reg(name, name, regType)

  def generateRegs(prefix: String)(range: Range, regType: RegType = Bits(32)): Map[String, Reg] =
    range.map(prefix + _).map(name => (name, Reg(name, regType))).toMap

  def generateRegs(names: String*)(regType: RegType): Map[String, Reg] =
    names.map(name => (name, Reg(name, regType))).toMap

}

object Reg32 {

  def apply(name: String, alias: String): Reg = new Reg(name, alias, Bits(32))

  def apply(name: String): Reg = apply(name, name)

}

object Reg64 {

  def apply(name: String, alias: String): Reg = new Reg(name, alias, Bits(64))

  def apply(name: String): Reg = apply(name, name)

}

class RegType {
  def widthHint(): Int = this match {
    case Bits(w) => w
    case Bit() => 1
    case Literal() => 0
  }
}

case class Bits(width: Int) extends RegType

case class Bit() extends RegType

case class Literal() extends RegType
