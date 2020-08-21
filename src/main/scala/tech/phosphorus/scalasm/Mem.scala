package tech.phosphorus.scalasm

import tech.phosphorus.scalasm.Basics.{Operand, Operated}

class MemLoad(val mem: Mem, val address: Operand, val length: Int) extends Operated {
  override val constant: Option[Basics.Constant] = None

  override def widthHint(system: System): Int = length
}

class MemStore

class Mem(val index: Int, val cellWidth: Int, val name: String) {
}



