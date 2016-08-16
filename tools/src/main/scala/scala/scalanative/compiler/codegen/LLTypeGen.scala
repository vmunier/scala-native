package scala.scalanative
package compiler
package codegen

import java.{lang => jl}
import util.{unsupported, unreachable, sh, Show}
import util.Show.{Sequence => s, Indent => i, Unindent => ui, Repeat => r, Newline => nl}
import nir._

trait LLTypeGen { self: LLCodeGen =>
  implicit val genType: Show[Type] = Show {
    case Type.Vararg                    => "..."
    case Type.Nothing                   => "void"
    case Type.Ptr | _: Type.RefKind     => "i8*"
    case Type.Bool                      => "i1"
    case Type.I8                        => "i8"
    case Type.I16                       => "i16"
    case Type.I32                       => "i32"
    case Type.I64                       => "i64"
    case Type.F32                       => "float"
    case Type.F64                       => "double"
    case Type.Array(ty, n)              => sh"[$n x $ty]"
    case Type.Function(args, Type.Unit) => sh"void (${r(args, sep = ", ")})"
    case Type.Function(args, ret)       => sh"$ret (${r(args, sep = ", ")})"
    case Type.Struct(Global.None, tys)  => sh"{ ${r(tys, sep = ", ")} }"
    case Type.Struct(name, _)           => sh"%$name"
    case ty                             => unsupported(ty)
  }
}
