package aarch64

import tech.phosphorus.scalasm
import tech.phosphorus.scalasm.Rtl.Reduced._
import tech.phosphorus.scalasm.Rtl._
import tech.phosphorus.scalasm.{Bits, Reg}

object Arch {
  val sys: scalasm.System = system { arch =>
    arch.name("Aarch64")
    arch.registers("X") {
      Reg.generateRegs("X")(0 to 30, Bits(64))
    }
    arch.registers("W") {
      Reg.generateRegs("W")(0 to 30, Bits(32))
    }

    arch.memory(index = 64, cellWidth = 8)

    /**
     * Mov-like instructions
     */
    arch.provideSemantics(Mov)

  }
}
