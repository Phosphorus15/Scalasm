package tech.phosphorus.scalasm

import scodec.bits.BitVector
import tech.phosphorus.scalasm.Basics.{DisasmVar, NamedImmediate, Operand, TypedRegister}
import tech.phosphorus.scalasm.RtlSema.InstructionEffectCollector
import tech.phosphorus.scalasm.System.InstructionSemantics
import tech.phosphorus.scalasm.insn.{FixedSegment, ImmediateSegment, InstructionSegment, RegSegment, SemanticsSegment, VarSegment}

import scala.collection.mutable

object Rtl {

  abstract class RegisterLoader {

    def select()

  }

  class SystemBuilder {

    val system: System = new System

    def registers(name: String)(eval: => Map[String, Reg]): Unit = {
      system.registerFile(name.toLowerCase) = eval
    }

    def memory(index: Int, cellWidth: Int): Unit =
      system.setPrimaryMem(new Mem(index, cellWidth, "main"))

    def memoryExtend(index: Int, cellWidth: Int, name: String): Unit =
      system.memoryFile(name) = new Mem(index, cellWidth, name)

    def alias(updater: PartialFunction[String, String]): Unit = {
      system.registerFile.mapValuesInPlace { (_, regs) =>
        regs.toIndexedSeq.map { case (_, reg) =>
          if (updater.isDefinedAt(reg.alias))
            (reg.name, reg.updateAlias(updater(reg.alias)))
          else
            (reg.name, reg)
        }.toMap
      }
    }

    def provideSemantics(provider: Instructions): Unit = {
      system.semantics ++= provider.semantics
    }

    def name(name: String): Unit = {
      system.name = name
    }
  }

  def system(eval: SystemBuilder => Unit): System = {
    val builder = new SystemBuilder
    eval(builder)
    builder.system
  }

  object Reduced {

    def reg(name: String, regType: RegType): Map[String, Reg] =
      Map.newBuilder.addOne((name, Reg(name, regType))).result()

  }

  abstract class Effect

  class Instructions {

    val semantics: mutable.Map[String, InstructionSemantics] = mutable.Map()

    class InstructionBuilder() {

      val segments: mutable.ArrayDeque[InstructionSegment] = mutable.ArrayDeque()

      def putFixed(bitVector: BitVector): Unit = {
        segments.addOne(FixedSegment(bitVector.size.toInt, bitVector))
      }

      def getLength: Int =
        segments.map(_.width).sum

      def loadImmediate(width: Int): Operand = {
        segments.addOne(VarSegment(width))
        DisasmVar((getLength, getLength + width))
      }

    }

    class InstructionSemanticsBuilder() extends InstructionEffectCollector {

      val segments: mutable.ArrayDeque[SemanticsSegment] = mutable.ArrayDeque()

      def loadImmediate(width: Int, id: String = "imm" + segments.count(_.isInstanceOf[ImmediateSegment])): Operand = {
        segments.addOne(ImmediateSegment(width, id))
        NamedImmediate(width, id)
      }

      def loadRegister(reg: String, id: String = "reg" + segments.count(_.isInstanceOf[RegSegment])): Operand = {
        segments.addOne(RegSegment(reg, id))
        TypedRegister(reg, id)
      }

      def gatherAll(): (Seq[SemanticsSegment], Effect) =
        (segments.toSeq, gather())

    }

    def insn(name: String)(eval: InstructionBuilder => Unit): Unit = {

    }

    def insnSema(name: String)(eval: InstructionSemanticsBuilder => Unit): Unit = {
      semantics(name) = (new InstructionSemanticsBuilder().yieldSelf({ builder =>
        eval.apply(builder.asInstanceOf[InstructionSemanticsBuilder])
      }).asInstanceOf[InstructionSemanticsBuilder].gatherAll())
    }

  }

}
