package scala.scalanative
package sbtplugin

import sbtcrossproject._

class NativePlatform(val os: OS, val arch: Arch) extends CrossPlatform {
  def name = {
    val os = this.os match {
      case AnyOS => ""
      case other => "-" + other.toString.toLowerCase
    }
    val arch = this.arch match {
      case AnyArch => ""
      case other   => "-" + other.toString.toLowerCase
    }

    s"native$os$arch"
  }

  def plugin = Some(ScalaNativePlugin)
}

trait OS
object AnyOS extends OS
object MacOS extends OS
object Linux extends OS

trait Arch
trait Arch32 extends Arch
trait Arch64 extends Arch
trait ArchLE extends Arch
trait ArchBE extends Arch

object AnyArch extends Arch
object Any32   extends Arch
object Any64   extends Arch
object X86     extends Arch32 with ArchLE
object X86_64  extends Arch64 with ArchLE
