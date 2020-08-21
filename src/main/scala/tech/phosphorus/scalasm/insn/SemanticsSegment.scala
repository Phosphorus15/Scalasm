package tech.phosphorus.scalasm.insn

class SemanticsSegment(val id: String)

case class ImmediateSegment(val width: Int, override val id: String) extends SemanticsSegment(id)

case class RegSegment(val regClass: String, override val id: String) extends SemanticsSegment(id)
