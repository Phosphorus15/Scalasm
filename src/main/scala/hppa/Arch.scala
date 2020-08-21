package hppa

import tech.phosphorus.scalasm.Rtl.Reduced._
import tech.phosphorus.scalasm.{Bits, Reg}
import tech.phosphorus.scalasm.Rtl._

object Arch {
  val sys = system { arch =>
    arch.name("PA-RISC")
    arch.registers("GR") {
      Reg.generateRegs("gr")(0 to 31, Bits(64))
    }
    arch.registers("SR") {
      Reg.generateRegs("sr")(0 to 7, Bits(64))
    }
    arch.registers("PSW") {
      reg("psw", Bits(64))
    }
    arch.registers("CR") {
      Reg.generateRegs("cr")(0 to 31, Bits(64))
    }
    arch.alias {
      case "gr28" => "ret0"
    }

    arch.memory(index = 64, cellWidth = 8)


  }
}
