package tech.phosphorus.scalasm

import scodec.bits.BitVector
import tech.phosphorus.scalasm.Basics.{Bitv, Intermediate, Operand, Operated, Value}
import tech.phosphorus.scalasm.Rtl.{Effect, system}
import tech.phosphorus.scalasm.RtlSema.Arithmetic._

import scala.collection.mutable

object RtlSema {

  implicit def operatedAsOperand[T <: Operated](operated: T): Operand =
    Intermediate(operated)

  implicit def operatedAsRtl[T <: Operated](operated: T): RtlOperand =
    new RtlOperand(Intermediate(operated))

  implicit def intAsImmediate(imm: Int): RtlOperand =
    new RtlOperand(Value(Bitv(BitVector.fromInt(imm))))

  implicit class RtlOperand(val operand: Operand) extends AnyVal {

    def :=(rtlOperand: RtlOperand) = new MovEffect(operand, rtlOperand.operand)

    def +(rtlOperand: RtlOperand) = new OperatedAdd(operand, rtlOperand.operand)

    def *(rtlOperand: RtlOperand) = new OperatedMul(operand, rtlOperand.operand)

    def <>(rtlOperand: RtlOperand) = new OperatedBitEq(operand, rtlOperand.operand)

    def apply(range: Range) = new OperatedExtract(range, operand)

    def extend(to: Int, signed: Boolean = false) = new OperatedExtend(to, signed, operand)

    def signed_extend(to: Int): OperatedExtend = extend(to, signed = true)

    def negate() = new OperatedNeg(operand)

    def as(id: String) = new OperatedLetId(id, operand)

  }

  class InstructionEffectCollector {

    val effects: mutable.ArrayDeque[Effect] = mutable.ArrayDeque()

    def apply(effect: Effect): Unit = {
      effects.addOne(effect)
    }

    def yieldSelf[T >: InstructionEffectCollector](eval: T => Unit): InstructionEffectCollector = {
      eval.apply(this)
      this
    }

    def gather(): Effect =
      effects.size match {
        case 0 => new NoneEffect
        case 1 => effects(0)
        case _ => new SeqEffect(effects.toSeq)
      }

  }

  def when(operand: RtlOperand)(holds: InstructionEffectCollector => Unit)
          (otherwise: InstructionEffectCollector => Unit = { _ => }): MuxEffect =
    new MuxEffect(operand.operand, new InstructionEffectCollector().yieldSelf(holds).gather(),
      new InstructionEffectCollector().yieldSelf(otherwise).gather())

  object Arithmetic {

    abstract class OperatedUnary(val value: Operand) extends Operated {
      override def widthHint(system: System): Int = value.widthHint(system)
    }

    abstract class OperatedBinary(val lhs: Operand, val rhs: Operand) extends Operated {
      override val constant: Option[Basics.Constant] = None

      override def widthHint(system: System): Int =
        lhs.widthHint(system) max rhs.widthHint(system)
    }

    class OperatedAdd(lhs: Operand, rhs: Operand) extends OperatedBinary(lhs, rhs) {
      override val constant: Option[Basics.Constant] = None
    }

    class OperatedMul(lhs: Operand, rhs: Operand) extends OperatedBinary(lhs, rhs) {
      override val constant: Option[Basics.Constant] = None
    }

    class OperatedNeg(operand: Operand) extends OperatedUnary(operand) {
      override val constant: Option[Basics.Constant] = None
    }

    class OperatedExtract(val range: Range, val operand: Operand) extends Operated {
      override val constant: Option[Basics.Constant] = None

      override def widthHint(system: System): Int = range.size
    }

    class OperatedExtend(val toLength: Int, val signed: Boolean, val operand: Operand) extends Operated {
      override val constant: Option[Basics.Constant] = None

      override def widthHint(system: System): Int = toLength
    }

    class OperatedBitEq(lhs: Operand, rhs: Operand) extends OperatedBinary(lhs, rhs) {
      override val constant: Option[Basics.Constant] = None
    }

    class OperatedLetId(val name: String, val operand: Operand) extends OperatedUnary(operand) {
      override val constant: Option[Basics.Constant] = None
    }

  }


}
