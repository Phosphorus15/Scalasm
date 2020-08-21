package tech.phosphorus.scalasm.bap

import tech.phosphorus.scalasm.Basics._
import tech.phosphorus.scalasm.Rtl._
import tech.phosphorus.scalasm.RtlSema.Arithmetic._
import tech.phosphorus.scalasm.{MovEffect, MuxEffect, NoneEffect, RegType, SeqEffect, System}

import scala.collection.mutable

class IndentationControl(val step: Int = 0) extends AnyVal {

  def next: IndentationControl = new IndentationControl(step + 1)

  def next(step: Int): IndentationControl = new IndentationControl(step + this.step)

  def getPadding: String = " ".repeat(step)

}

/**
 * This is a semantics generator complies to Carnegie Mellon University BAP's
 * Core Theory design, with build-in pretty print functions,
 * and is expected to be interpreted by dsl_common.ml file created by @Phosphorus15
 */
class LetCollector {

  val bitsVar: mutable.Map[Int, mutable.Set[(String, Operand)]] = mutable.Map()

  def collectVar(id: String, reg: Int, operand: Operand): Unit =
    bitsVar.updateWith(reg) {
      set => Some(set.getOrElse(mutable.Set()).addOne((id, operand)))
    }

  def getLetScoped: String =
    (bitsVar.keys.filter(_ != 0)
      .map(width => s"let bits$width = Theory.Bitv.define $width in") ++
      bitsVar.toSeq.flatMap { case (width, value) =>
        value.map { case (id, _) =>
          s"local_var_sort bits$width >>= fun $id ->"
        }
      }).mkString("\n")

}

class SemanticsGenerator(arch: ArchGenerator, system: System) {

  class FormatString(val text: String, val multiSeg: Boolean) {
    override def toString: String = if (multiSeg) s"($text)" else text

    def hotReplace(newText: String => String) =
      new FormatString(newText.apply(text), multiSeg)
  }

  implicit def formatFromTuple(tuple: (String, Boolean)): FormatString =
    new FormatString(tuple._1, tuple._2)

  implicit def formatFromString(text: String): FormatString = {
    new FormatString(text, text.contains(' '))
  }

  {
    system.semantics.foreach { case (name, sema) =>
      generateFrom(name, sema)
    }
  }

  def generateFrom(name: String, semantics: System.InstructionSemantics): Unit = {
    semantics match {
      case (params, effect) =>
        val letCollector = new LetCollector
        println(resolveEffect(new IndentationControl(), letCollector)(effect).text)
        println(letCollector.getLetScoped)
    }
  }

  def resolveEffect(indent: IndentationControl, letCollector: LetCollector)(effect: Effect): FormatString = {
    (effect match {
      case _: NoneEffect => "bot"
      case seqEffect: SeqEffect => "[\n" +
        seqEffect.effects.map(resolveEffect(indent.next(2), letCollector)).map(_.text).mkString(";\n") +
        s"\n${indent.next.getPadding}]"
      case assignEffect: MovEffect =>
        resolveAssignee(assignEffect.assignee) + " := " + resolveValueBase(assignEffect.assigner, letCollector, indent.next).text
      case muxEffect: MuxEffect =>
        val holdsMux = muxEffect.holds.isInstanceOf[SeqEffect]
        val otherwiseMux = muxEffect.otherwise.isInstanceOf[SeqEffect]
        val holds = resolveEffect(indent.next(if (holdsMux) 0 else 2), letCollector)(muxEffect.holds).text
        val holdsEff = if (holdsMux) holds else s"[${holds.stripLeading}]"
        val otherwise = resolveEffect(indent.next(if (otherwiseMux) 0 else 2), letCollector)(muxEffect.otherwise).text
        val otherwiseEff = if (otherwiseMux) otherwise else s"[${otherwise.stripLeading}]"
        s"if_ (${resolveValueBase(muxEffect.cond, letCollector, indent.next).text}) " +
          s"\n${indent.next.getPadding}$holdsEff " +
          s"\n${indent.next.getPadding}$otherwiseEff"
      case _ => ""
    }) hotReplace { it =>
      indent.getPadding + it
    }
  }

  def resolveValueBase(operand: Operand, letCollector: LetCollector, indentationControl: IndentationControl): FormatString = {
    val scopeCollector = new LetCollector
    val enclosure = resolveValue(operand, scopeCollector)
    if (scopeCollector.bitsVar.isEmpty) enclosure
    else
      fixTrailingIndent(indentationControl,
        scopeCollector.bitsVar.toSeq.flatMap { case (width, value) =>
          value.map { case (id, operand) =>
            (width, id, operand)
          }
        }.foldLeft(enclosure + "\n") { case (enclosed, (width, id, operand)) =>
          val let = s"let_ $id ${resolveValueBase(operand, letCollector, indentationControl.next)} (\n" +
            s"${indentationControl.getPadding}${enclosed.text})"
          s"Theory.Var.scoped bits$width (fun $id ->\n" +
            s"${indentationControl.getPadding}$let)"
        })
  }

  def fixTrailingIndent(indentationControl: IndentationControl, string: String): String = {
    val lines = string.linesWithSeparators.toSeq
    val trailing = indentationControl.getPadding + lines.last
    lines.init.concat(trailing).mkString
  }

  def resolveValue(operand: Operand, letCollector: LetCollector): FormatString = {
    operand match {
      case Register(reg) => s"var ${reg.name}"
      case RegisterUnresolved(reg) => s"var $reg" // TODO resolve the register
      case TypedRegister(_, id) => id
      case Value(Bitv(bitv)) => s"!!${bitv.toInt()}"
      case Intermediate(operated) => resolveOperated(operated, letCollector)
      case NamedImmediate(_, id) => id
    }
  }

  def resolveOperated(operated: Operated, letCollector: LetCollector): FormatString = {
    def resolveValue(operand: Operand) = this.resolveValue(operand, letCollector)

    operated match {
      case bitEq: OperatedBitEq =>
        s"${resolveValue(bitEq.lhs)} <> ${resolveValue(bitEq.rhs)}"
      case add: OperatedAdd =>
        s"${resolveValue(add.lhs)} + ${resolveValue(add.rhs)}"
      case mul: OperatedMul =>
        s"${resolveValue(mul.lhs)} * ${resolveValue(mul.rhs)}"
      case extract: OperatedExtract =>
        s"extract !!${extract.range.start} !!${extract.range.`end`} ${resolveValue(extract.operand)}"
      case extend: OperatedExtend =>
        val extend_style = if(extend.signed) "signed" else "unsigned"
        s"$extend_style bits${extend.toLength} ${resolveValue(extend.operand)}"
      case let: OperatedLetId =>
        letCollector.collectVar(let.name, let.operand.widthHint(system), let.operand)
        let.name
      case _ => "<unresolved>"
    }
  }

  def resolveAssignee(operand: Operand): FormatString = {
    operand match {
      case Register(reg) => reg.name // TODO concat with def module name
      case TypedRegister(_, id) => id
      case _ => "<error>"
    }
  }

}
