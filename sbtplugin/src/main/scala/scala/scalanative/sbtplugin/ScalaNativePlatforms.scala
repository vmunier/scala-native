package scala.scalanative
package sbtplugin

import compiler._, Target._
import sbtcrossproject._

class NativePlatform(val os: OS, val arch: Arch) extends CrossPlatform {
  def target: compiler.Target = new compiler.Target(os, arch)

  def name: String = {
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


