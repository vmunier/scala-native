package scala.scalanative
package compiler
package codegen

import java.{lang => jl}
import scala.collection.mutable
import util.{unsupported, unreachable, sh, Show}
import util.Show.{Sequence => s, Indent => i, Unindent => ui, Repeat => r, Newline => nl}
import nir._
import compiler.analysis.ClassHierarchy._
import compiler.analysis.ClassHierarchyExtractors._

trait LLValGen { self: LLCodeGen =>
  import LLValGen._

  private val consts = mutable.Map.empty[Val, Res]
  private var constid = 0

  private lazy val stringFieldNames = {
    val node  = ClassRef.unapply(StringName).get
    val names = node.allvars.sortBy(_.index).map(_.name)
    assert(names.length == 4, "java.lang.String is expected to have 4 fields.")
    names
  }

  private def llvmFloatHex(value: Float): String =
    "0x" + jl.Long.toHexString(jl.Double.doubleToRawLongBits(value.toDouble))

  private def llvmDoubleHex(value: Double): String =
    "0x" + jl.Long.toHexString(jl.Double.doubleToRawLongBits(value))

  def genJustVal(v: Val): Res = v match {
    case Val.Unit                            => genJustVal(LLDefnGen.unit)
    case Val.True                            => "true"
    case Val.False                           => "false"
    case Val.Zero(ty)                        => "zeroinitializer"
    case Val.Undef(ty)                       => "undef"
    case Val.I8(v)                           => v.toString
    case Val.I16(v)                          => v.toString
    case Val.I32(v)                          => v.toString
    case Val.I64(v)                          => v.toString
    case Val.F32(v)                          => llvmFloatHex(v)
    case Val.F64(v)                          => llvmDoubleHex(v)
    case Val.Struct(_, vs)                   => sh"{ ${r(vs, sep = ", ")} }"
    case Val.Array(_, vs)                    => sh"[ ${r(vs, sep = ", ")} ]"
    case Val.Chars(v)                        => s("c\"", v, "\\00", "\"")
    case Val.Const(v)                        => genConst(v)
    case Val.String(v)                       => genString(v)
    case Val.Local(n, _) if copy.contains(n) => genJustVal(copy(n))
    case Val.Local(n, _)                     => sh"%$n"
    case Val.Global(n, _) =>
      world.nodes(n) match {
        case node: Scope  => typeConst(node)
        case node: Method => sh"bitcast (${node.ty}* @$n to i8*)"
        case node: Var    => sh"bitcast (${node.ty}* @$n to i8*)"
        case node: Const  => sh"bitcast (${node.ty}* @$n to i8*)"
      }
    case _ =>
      unsupported(v)
  }

  def genConst(v: Val): Res = {
    if (consts.contains(v)) {
      consts(v)
    } else {
      val id = constid
      constid += 1
      ll.global(Global.Top("__const." + id), v.ty, genJustVal(v))
      val res = sh"bitcast (${v.ty}* @__const.$id to i8*)"
      consts(v) = res
      res
    }
  }

  def genString(v: String): Res = {
    val StringCls    = ClassRef.unapply(StringName).get
    val CharArrayCls = ClassRef.unapply(CharArrayName).get

    val chars       = v.toCharArray
    val charsLength = Val.I32(chars.length)
    val charsConst = Val.Const(
        Val.Struct(
            Global.None,
            Seq(CharArrayRtty,
                charsLength,
                Val.I32(0), // padding to get next field aligned properly
                Val.Array(Type.I16, chars.map(c => Val.I16(c.toShort))))))

    val fieldValues = stringFieldNames.map {
      case StringValueName          => charsConst
      case StringOffsetName         => Val.I32(0)
      case StringCountName          => charsLength
      case StringCachedHashCodeName => Val.I32(v.hashCode)
      case _                        => util.unreachable
    }

    genConst(Val.Struct(Global.None, StringRtty +: fieldValues))
  }

  def genJustGlobal(n: Global) = n match {
    case Ref(node) if node.attrs.isExtern => sh"@${stripExtern(n)}"
    case _                                => sh"@$n"
  }

  implicit val genVal: Show[Val] = Show { v =>
    sh"${v.ty} ${genJustVal(v)}"
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

  implicit val genLocal: Show[Local] = Show {
    case Local(scope, id) => sh"$scope.$id"
  }
}

object LLValGen extends Depends {
  val StringName               = Rt.String.name
  val StringRtty               = Val.Global(StringName, Type.Ptr)
  val StringValueName          = StringName member "value" tag "field"
  val StringOffsetName         = StringName member "offset" tag "field"
  val StringCountName          = StringName member "count" tag "field"
  val StringCachedHashCodeName = StringName member "cachedHashCode" tag "field"

  val CharArrayName = Global.Top("scala.scalanative.runtime.CharArray")
  val CharArrayRtty = Val.Global(CharArrayName, Type.Ptr)

  override val depends = Seq(StringName,
                             StringValueName,
                             StringOffsetName,
                             StringCountName,
                             StringCachedHashCodeName,
                             CharArrayName)
}
