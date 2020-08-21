package tech.phosphorus.scalasm

import tech.phosphorus.scalasm.Rtl.Effect
import tech.phosphorus.scalasm.System.InstructionSemantics
import tech.phosphorus.scalasm.insn.SemanticsSegment

import scala.collection.mutable

object System {
  type InstructionSemantics = (Seq[SemanticsSegment], Effect)
}

class System(var name: String = "<anonymous>") {

  val semantics: mutable.Map[String, InstructionSemantics] = mutable.Map()

  val registerFile: mutable.Map[String, Map[String, Reg]] = mutable.Map()

  def allRegisters(): Iterable[Reg] =
    registerFile.values.flatMap(_.values)

  def findRegister(name: String): Option[Reg] =
    allRegisters().find(reg => reg.alias == name || reg.name == name)

  val memoryFile: mutable.Map[String, Mem] = mutable.Map()

  def setPrimaryMem(mem: Mem): Unit = memoryFile("main") = mem

  def getPrimaryMem: Mem = memoryFile("main")

}
