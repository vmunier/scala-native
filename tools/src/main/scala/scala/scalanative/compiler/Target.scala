package scala.scalanative
package compiler

import Target._

/** Structured representation of LLVM's target triple. */
class Target(val os: OS, val arch: Arch) {
  def triple: String = {
    val os   = if (this.os == AnyOS) current.os else this.os
    val arch = if (this.arch == AnyArch) current.arch else this.arch
    s"${arch.name}-${os.vendor}-${os.system}"
  }
}

object Target {
  val current = new Target(Darwin, X86_64)

  trait OS {
    def vendor: String
    def system: String
  }

  object AnyOS  extends OS { def vendor = "";      def system = "" }
  object Darwin extends OS { def vendor = "apple"; def system = "darwin" }
  object Linux  extends OS { def vendor = "pc";    def system = "linux" }

  trait Arch {
    def name: String
  }

  trait Arch32 extends Arch
  trait Arch64 extends Arch
  trait ArchLE extends Arch
  trait ArchBE extends Arch

  object AnyArch extends Arch               { def name: String = "" }
  object Any32   extends Arch32             { def name: String = "" }
  object Any64   extends Arch64             { def name: String = "" }
  object X86     extends Arch32 with ArchLE { def name: String = "x86" }
  object X86_64  extends Arch64 with ArchLE { def name: String = "x86_64" }
}
