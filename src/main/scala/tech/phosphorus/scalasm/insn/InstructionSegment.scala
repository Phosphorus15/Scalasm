package tech.phosphorus.scalasm.insn

import scodec.bits.BitVector

class InstructionSegment(val width: Int)

case class FixedSegment(override val width: Int, val bits: BitVector) extends InstructionSegment(width)

case class VarSegment(override val width: Int) extends InstructionSegment(width)