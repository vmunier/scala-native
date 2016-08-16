package scala.scalanative
package compiler
package codegen

import scala.collection.mutable.{UnrolledBuffer => Buf, Map}
import util.{unsupported, unreachable, sh, Show}
import util.Show.{Sequence => s, Indent => i, Unindent => ui, Repeat => r, Newline => nl, Result => Res}
import nir._, Shows.brace

final class LLBuilder(fresh: Fresh) {

  // Current definition state

  private val declares = Buf.empty[Res]
  private val defines  = Buf.empty[Res]
  private val consts   = Buf.empty[Res]
  private val globals  = Buf.empty[Res]
  private val structs  = Buf.empty[Res]
  private val tys      = Map.empty[Global, Res]

  // Current body state

  private val edges    = Map.empty[Local, Buf[(Local, Seq[Res])]]
  private val blocks   = Buf.empty[LLBlock]
  private val handlers = Map.empty[Local, Option[Local]]

  // Current basic block state

  private var name: Local               = _
  private var params: Seq[(Local, Res)] = _
  private var insts: Buf[Res]           = _
  private var isEntry: Boolean          = _
  private var handler: Option[Local]    = None

  // Helpers

  private def edge(to: Local, values: Seq[Res] = Seq.empty): Unit = {
    if (!edges.contains(to)) {
      edges(to) = Buf.empty
    }
    edges(to) += ((name, values))
  }

  private def exchandler(to: Local, handler: Local): Unit =
    handlers(to) = Some(handler)

  private implicit val showLocal: Show[Local] = Show {
    case Local(scope, id) => sh"$scope.$id"
  }

  implicit val genGlobal: Show[Global] = Show { g =>
    def quoted(sh: Res) =
      s("\"", sh, "\"")
    def justGlobal(g: Global): Res = g match {
      case Global.None          => unsupported(g)
      case Global.Top(id)       => id
      case Global.Member(n, id) => s(justGlobal(n), "::", id)
    }
    quoted(justGlobal(g))
  }

  // Definitions

  def start() = {
    declares.clear()
    defines.clear()
    consts.clear()
    globals.clear()
    structs.clear()
  }

  def end(): Res = {
    val res = Buf.empty[Res]
    res ++= structs
    res ++= consts
    res ++= globals
    res ++= declares
    res ++= defines
    r(res, sep = nl(""))
  }

  def declare(retty: Res,
              name: Global,
              argtys: Seq[Res],
              attrs: Seq[Res]): Unit = {
    val arglist  = r(argtys, sep = ", ")
    val attrlist = r(attrs, sep = " ", pre = " ")
    declares += sh"declare $retty @$name($arglist)$attrlist"
    tys(name) = sh"$retty ($arglist)"
  }

  def define(retty: Res,
             name: Global,
             argtys: Seq[Res],
             attrs: Seq[Res],
             body: Res): Unit = {
    val arglist  = r(argtys, sep = ", ")
    val attrlist = r(attrs, sep = " ", pre = " ")
    defines += sh"define $retty @$name($arglist)$attrlist ${brace(body)}"
    tys(name) = sh"$retty ($arglist)"
  }

  def global(name: Global,
             ty: Res,
             init: Res,
             isConst: Boolean = false,
             isExtern: Boolean = false): Unit = {
    val keyword  = if (isConst) "constant" else "global"
    val external = if (isExtern) "external " else ""
    globals += sh"@$name = ${external}global $ty$init"
    tys(name) = ty
  }

  def struct(name: Global, tys: Seq[Res]): Unit = {
    structs += sh"%$name = type {${r(tys, sep = ", ")}}"
  }

  // Basic blocks

  def startBody(): Unit = {
    edges.clear()
    blocks.clear()
    handlers.clear()
    handler = None
  }

  def endBody(): Res = {
    val buf = Buf.empty[Res]

    blocks.foreach {
      case LLBlock(name, params, insts, isEntry) =>
        buf += sh"${nl(name)}:"

        if (!isEntry) {
          val preds = edges.getOrElse(name, Buf.empty)
          params.zipWithIndex.map {
            case ((name, ty), n) =>
              val froms = preds.map {
                case (from, vals) =>
                  sh"[${vals(n)}, %$from]"
              }

              buf += i(sh"%$name = phi $ty ${r(froms, sep = ", ")}")
          }
        }

        insts.foreach { inst =>
          buf += i(inst)
        }
    }

    r(buf).toString
  }

  def block(name: Local,
            params: Seq[(Local, Res)] = Seq(),
            isEntry: Boolean = false): Unit = {
    this.name = name
    this.params = params
    this.insts = Buf.empty[Res]
    this.isEntry = isEntry
    this.handler = handlers.getOrElse(name, this.handler)
    blocks += LLBlock(name, params, this.insts, isEntry)
  }

  // Instructions

  def inst(op: Res): Unit =
    insts += op

  def inst(name: Local, op: Res): Unit =
    insts += sh"%$name = $op"

  def invoke(sig: Res): Unit = {
    handler.fold[Unit] {
      insts += sh"call $sig"
    } { eh =>
      val succ = fresh()
      insts += sh"invoke $sig to label %$succ unwind label %$eh"
      block(succ)
    }
  }

  def invoke(name: Local, sig: Res): Unit = {
    handler.fold[Unit] {
      insts += sh"%$name = call $sig"
    } { eh =>
      val succ = fresh()
      insts += sh"%$name = invoke $sig to label %$succ unwind label %$eh"
      block(succ)
    }
  }

  def unreachable(): Unit =
    insts += sh"unreachable"

  def ret(value: Res): Unit =
    insts += sh"ret $value"

  def resume(value: Res): Unit = {
    insts += sh"resume $value"
  }

  def jump(next: Local,
           values: Seq[Res] = Seq.empty,
           eh: Option[Local] = None): Unit = {
    insts += sh"br label %$next"
    edge(next, values)
    eh.foreach(exchandler(next, _))
  }

  def branch(cond: Res, thenp: Local, elsep: Local): Unit = {
    insts += sh"br $cond, label %$thenp, label %$elsep"
    edge(thenp)
    edge(elsep)
  }

  def switch(scrut: Res, default: Local, cases: Seq[(Res, Local)]): Unit = {
    val pairs = cases.map {
      case (v, n) =>
        edge(n)
        sh"$v, label %$n"
    }
    insts += sh"switch $scrut, label %$default [${r(pairs.map(i(_)))}${nl("]")}"
  }

  // Values

  def global(n: Global): Res = sh"${tys(n)}* @$n"
}

final case class LLBlock(name: Local,
                         params: Seq[(Local, Res)],
                         insts: Buf[Res],
                         isEntry: Boolean)
