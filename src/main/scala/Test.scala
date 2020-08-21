import hppa.Arch
import tech.phosphorus.scalasm.bap.{ArchGenerator, SemanticsGenerator}
import tech.phosphorus.scalasm.{Bits, Reg}

import scala.language.experimental.macros

object Test extends App {

  implicit class InjectInt(i: Int) {
    def inject(a: Int): Int = i + a
  }

  new SemanticsGenerator(new ArchGenerator, aarch64.Arch.sys)
}