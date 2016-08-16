package scala.scalanative
package compiler

import java.lang.System.{currentTimeMillis => time}
import java.nio.ByteBuffer
import scala.collection.mutable
import codegen.LLCodeGen
import linker.Linker
import nir._, Shows._
import nir.serialization._
import util.sh
import transform._

final class Compiler(opts: Opts) {
  private lazy val entry =
    Global.Member(Global.Top(opts.entry), "main_class.ssnr.ObjectArray_unit")

  private def measure[T](label: String)(f: => T): T = {
    val start = time()
    val res   = f
    val end   = time()
    println(s"-- $label: ${end - start} ms")
    res
  }

  private lazy val depends = Seq(codegen.LLValGen, codegen.LLDefnGen)

  private lazy val (links, assembly): (Seq[Attr.Link], Seq[Defn]) =
    measure("linking") {
      val deps           = depends.flatMap(_.depends).distinct
      val injects        = depends.flatMap(_.injects).distinct
      val linker         = new Linker(opts.dotpath, opts.classpath)
      val (links, defns) = linker.linkClosed(entry +: deps)
      val assembly       = defns ++ injects

      if (opts.verbose) dump(assembly, ".hnir")

      (links, defns ++ injects)
    }

  private lazy val ctx = Ctx(fresh = Fresh("tx"),
                             entry = entry,
                             world = analysis.ClassHierarchy(assembly))

  private lazy val transform = Transform.compose(DeadCodeElimination(ctx),
                                                 LocalBoxingElimination(ctx),
                                                 StackallocHoisting(ctx))

  private def gencode(assembly: Seq[Defn]): Unit = measure("codegen") {
    def serialize(defns: Seq[Defn], bb: ByteBuffer): Unit = {
      val gen = new LLCodeGen(assembly, ctx.entry, transform)(ctx.world)
      gen.gen(bb)
    }
    serializeFile(serialize _, assembly, opts.outpath)
  }

  private def dump(assembly: Seq[Defn], suffix: String) = {
    def serialize(defns: Seq[Defn], bb: ByteBuffer): Unit = {
      bb.put(nir.Shows.showDefns(assembly).toString.getBytes)
    }
    serializeFile(serialize _, assembly, opts.outpath + suffix)
  }

  def apply(): Seq[Attr.Link] = measure("total") {
    val assembly = this.assembly

    gencode(assembly)

    links
  }
}
