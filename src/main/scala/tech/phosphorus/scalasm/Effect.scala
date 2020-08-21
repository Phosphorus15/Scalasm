package tech.phosphorus.scalasm

import tech.phosphorus.scalasm.Basics.Operand
import tech.phosphorus.scalasm.Rtl.Effect

class NoneEffect extends Effect

class SeqEffect(val effects: Seq[Effect]) extends Effect {

  override def toString = s"SeqEffect(effects=$effects)"

}

class MovEffect(val assignee: Operand, val assigner: Operand) extends Effect {

  override def toString = s"MovEffect(assignee=$assignee, assigner=$assigner)"

}

class MuxEffect(val cond: Operand, val holds: Effect, val otherwise: Effect) extends Effect {

  override def toString = s"MuxEffect(cond=$cond, holds=$holds, otherwise=$otherwise)"
}
