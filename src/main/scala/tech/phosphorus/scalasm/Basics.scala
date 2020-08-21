package tech.phosphorus.scalasm

import scodec.bits.BitVector

object Basics {

  class Constant

  case class Bitv(value: BitVector) extends Constant {
    def width: Long = value.size
  }

  case class Str(name: String) extends Constant

  abstract class Operated {
    val constant: Option[Constant]

    def widthHint(system: System): Int
  }

  class Operand {
    def widthHint(system: System): Int = 0
  }

  case class RegisterUnresolved(id: String) extends Operand {
    override def widthHint(system: System): Int =
      system.findRegister(id).map(_.regType.widthHint()).getOrElse(0)
  }

  case class Register(reg: Reg) extends Operand {
    override def widthHint(system: System): Int = reg.regType.widthHint()
  }

  case class Value(value: Constant) extends Operand {
    override def widthHint(system: System): Int = value match {
      case Bitv(bitv) => bitv.size.toInt
      case Str(_) => 0
    }
  }

  case class DisasmVar(source: (Int, Int)) extends Operand {
    override def widthHint(system: System): Int = source._2 - source._1 + 1
  }

  case class MappedRegister(disasmVar: DisasmVar, mapper: Int => (Reg, String)) extends Operand {
    /**
     * tries to get the first possible mapped register
     *
     * @return
     */
    override def widthHint(system: System): Int = mapper.apply(0)._1.regType.widthHint()
  }

  case class NamedImmediate(width: Int, id: String) extends Operand {
    override def widthHint(system: System): Int = width
  }

  case class TypedRegister(regClass: String, id: String) extends Operand {
    override def widthHint(system: System): Int =
      system.registerFile(regClass.toLowerCase).values.headOption
        .map(_.regType.widthHint()).getOrElse(0)
  }

  case class Intermediate(operated: Operated) extends Operand {
    override def widthHint(system: System): Int = operated.widthHint(system)
  }

}
