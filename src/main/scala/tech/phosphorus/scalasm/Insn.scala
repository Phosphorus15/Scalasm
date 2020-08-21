package tech.phosphorus.scalasm

import scodec.bits.BitVector

class Insn(val segments: List[InsnSegment]) {


}

class InsnSegment(val width: Int)

case class InsnFixed(val width: Int, val bits: BitVector)

case class InsnVar(val width: Int)
