package aarch64

import tech.phosphorus.scalasm.Basics.{Register, RegisterUnresolved}
import tech.phosphorus.scalasm.Rtl.Instructions
import tech.phosphorus.scalasm.RtlSema.{RtlOperand, operatedAsRtl, when}
import tech.phosphorus.scalasm.SeqEffect

object Mov extends Instructions{

  insnSema("MOVXr") { insn =>
    val Rm = insn.loadRegister("W")
    val Rd = insn.loadRegister("W")

    insn(Rm := Rd)
  }

  insnSema("MOVXr") { insn =>
    val Rm = insn.loadRegister("X")
    val Rd = insn.loadRegister("X")

    insn(Rm := Rd)
  }

  insnSema("MADD") { insn =>
    val sf = insn.loadImmediate(1)
    val Rm = insn.loadRegister("W") extend 64 as "rm"
    val Rn = insn.loadRegister("W") extend 64 as "rn"
    val Ra = insn.loadRegister("W") extend 64 as "ra"
    val Rd = insn.loadRegister("W")
    val Rs = RegisterUnresolved("W30")

    insn(when(sf <> 1) { b64 =>
      b64(
        new SeqEffect(Seq(
          Rd := (Ra + (Rn * Rm)) (0 to 63),
          Rd := Rd + Ra + Rs
        ))
      )
    } { b32 =>
      b32(Rd := (Ra + (Rn * Rm)) (0 to 31))
    })
  }

  insnSema("SUBXrs") { insn =>

  }

}
