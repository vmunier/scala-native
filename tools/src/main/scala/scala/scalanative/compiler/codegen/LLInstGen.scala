package scala.scalanative
package compiler
package codegen

import scala.collection.mutable
import util.{unsupported, unreachable, sh, Show}
import util.Show.{Sequence => s, Indent => i, Unindent => ui, Repeat => r, Newline => nl}
import nir._
import compiler.analysis._
import ClassHierarchy._
import ClassHierarchyExtractors._

trait LLInstGen { self: LLCodeGen =>
  import LLInstGen._
  import self.{instanceTy => ity, instance => ival, dispatchTy => dty, dispatch => dval}

  private var returnsVoid = false
  private def terminate(): Nothing =
    throw Terminate
  private def terminating(f: => Unit): Unit =
    try f
    catch {
      case Terminate => ()
    }

  protected lazy val rtty = genType(Rt.Type)
  protected lazy val unit = genJustVal(Val.Unit)
  protected lazy val copy = mutable.Map.empty[Local, Val]

  protected def typeConst(node: Node) = node match {
    case cls: Class =>
      val name = node.name tag "type"
      val ty   = cls.typeStruct: Type
      sh"bitcast ($ty* ${genJustGlobal(name)} to i8*)"
    case _ =>
      val name = node.name tag "type"
      sh"bitcast ($rtty* ${genJustGlobal(name)} to i8*)"
  }

  def genBody(blocks: Seq[Block], returnsVoid: Boolean): Res = {
    ll.startBody()

    this.returnsVoid = returnsVoid
    val cfg = ControlFlow(blocks)
    cfg.map { node =>
      val block @ Block(name, params, insts, cf) = node.block

      val pairs = params.map { case Val.Local(n, ty) => (n, (ty: Res)) }

      ll.block(name, pairs, isEntry = node eq cfg.entry)
      terminating {
        insts.foreach { inst =>
          genInst(inst.name, inst.op)
        }
        genCf(name, cf)
      }

      cf match {
        case cf: Cf.Try => genLandingPad(name, cf)
        case _          => ()
      }
    }

    copy.clear()
    ll.endBody()
  }

  def genLandingPad(in: Local, cf: Cf.Try): Unit = {
    val landingpad      = in tag "landingpad"
    val resume          = in tag "resume"
    val exc             = in tag "exc"
    val rec, rec0, rec1 = fresh()
    val recid, reccmp   = fresh()
    val wrap0, wrap1    = fresh()

    ll.block(landingpad)
    ll.inst(rec, LLInstGen.landingpad)
    ll.inst(rec0, sh"extractvalue $excrecty %$rec, 0")
    ll.inst(rec1, sh"extractvalue $excrecty %$rec, 1")
    ll.inst(recid, sh"$typeid")
    ll.inst(reccmp, sh"icmp eq i32 %$rec1, %$recid")
    ll.inst(wrap0, sh"bitcast i8* %$rec0 to i8**")
    ll.inst(wrap1, sh"getelementptr i8*, i8** %$wrap0, i32 1")
    ll.inst(exc, sh"load i8*, i8** %$wrap1")
    ll.branch(sh"i1 %$reccmp", in tag "catch.0", resume)

    ll.block(resume)
    ll.resume(sh"$excrecty %$rec")

    val fails = (1 to cf.catches.length).tail.map { n =>
      in tag s"catch.${n - 1}"
    } :+ resume

    cf.catches.zip(fails).zipWithIndex.foreach {
      case ((ctch @ Next.Catch(ty, succ), fail), n) =>
        val catchn     = in tag s"catch.$n"
        val catchnsucc = catchn tag "succ"
        val cond       = fresh()

        ll.block(catchn)
        genIs(cond, ty, sh(sh"i8* %$exc"))
        ll.branch(sh"i1 %$cond", catchnsucc, fail)

        ll.block(catchnsucc)
        ll.invoke(sh"i8* @__cxa_begin_catch(i8* %$rec0)")
        ll.invoke(sh"void @__cxa_end_catch()")
        ll.jump(succ, Seq(sh"%$exc"))
    }
  }

  def genCf(in: Local, cf: Cf): Unit =
    cf match {
      case Cf.Unreachable =>
        ll.unreachable()

      case Cf.Ret(_) if returnsVoid =>
        ll.ret(sh"void")

      case Cf.Ret(value) =>
        ll.ret(value)

      case Cf.Jump(Next.Label(name, values)) =>
        ll.jump(name, values.map(genJustVal))

      case Cf.If(cond, thenp, elsep) =>
        ll.branch(cond, thenp.name, elsep.name)

      case Cf.Switch(scrut, default, cases) =>
        val pairs = cases.map {
          case Next.Case(v, n) => (s(v), n)
          case _               => unreachable
        }
        ll.switch(scrut, default.name, pairs)

      case Cf.Throw(value) =>
        ll.invoke(sh"void @scalanative_throw($value)")
        ll.unreachable()

      case Cf.Try(default, cases) =>
        ll.jump(default.name, eh = Some(in tag "landingpad"))
    }

  def genInst(name: Local, op: Op): Unit = {
    if (op.resty.isUnit) {
      copy(name) = Val.Unit
    }

    op match {
      case Op.Call(ty, ptr, args) =>
        genCall(name, ty, ptr, args)

      case Op.Load(ty, ptr) =>
        val pointee = fresh()

        ll.inst(pointee, sh"bitcast $ptr to $ty*")
        ll.inst(name, sh"load $ty, $ty* %$pointee")

      case Op.Store(ty, ptr, value) =>
        val pointee = fresh()

        ll.inst(pointee, sh"bitcast $ptr to $ty*")
        ll.inst(sh"store $value, $ty* %$pointee")

      case Op.Elem(ty, ptr, indexes) =>
        val pointee, derived = fresh()

        ll.inst(pointee, sh"bitcast $ptr to $ty*")
        ll.inst(
            derived,
            sh"getelementptr $ty, $ty* %$pointee, ${r(indexes, sep = ", ")}")
        ll.inst(name, sh"bitcast ${ty.elemty(indexes.tail)}* %$derived to i8*")

      case Op.Stackalloc(ty, n) =>
        val pointee = fresh()
        val elems   = if (n == Val.None) sh"" else sh", $n"

        ll.inst(pointee, sh"alloca $rtty$elems")
        ll.inst(name, sh"bitcast $rtty* %$pointee to i8*")

      case Op.Extract(aggr, indexes) =>
        ll.inst(name, sh"extractvalue $aggr, ${r(indexes, sep = ", ")}")

      case Op.Insert(aggr, value, indexes) =>
        ll.inst(name, sh"insertvalue $aggr, $value, ${r(indexes, sep = ", ")}")

      case Op.Bin(opcode, ty, l, r) =>
        val bin = opcode match {
          case Bin.Iadd => "add"
          case Bin.Isub => "sub"
          case Bin.Imul => "mul"
          case _        => opcode.toString.toLowerCase
        }

        ll.inst(name, sh"$bin $l, ${genJustVal(r)}")

      case Op.Comp(opcode, ty, l, r) =>
        val cmp = opcode match {
          case Comp.Ieq => "icmp eq"
          case Comp.Ine => "icmp ne"
          case Comp.Ult => "icmp ult"
          case Comp.Ule => "icmp ule"
          case Comp.Ugt => "icmp ugt"
          case Comp.Uge => "icmp uge"
          case Comp.Slt => "icmp slt"
          case Comp.Sle => "icmp sle"
          case Comp.Sgt => "icmp sgt"
          case Comp.Sge => "icmp sge"
          case Comp.Feq => "fcmp ueq"
          case Comp.Fne => "fcmp une"
          case Comp.Flt => "fcmp ult"
          case Comp.Fle => "fcmp ule"
          case Comp.Fgt => "fcmp ugt"
          case Comp.Fge => "fcmp uge"
        }

        ll.inst(name, sh"$cmp $l, ${genJustVal(r)}")

      case Op.Conv(opcode, ty, v) =>
        ll.inst(name, sh"$opcode $v to $ty")

      case Op.Select(cond, v1, v2) =>
        ll.inst(name, sh"select $cond, $v1, $v2")

      case Op.Classalloc(ClassRef(cls)) =>
        val size  = fresh()
        val clsty = typeConst(cls)

        genInst(size, Op.Sizeof(cls.classStruct))
        ll.invoke(name, sh"i8* @scalanative_alloc(i8* $clsty, i64 %$size)")

      case Op.Field(ty, obj, VarRef(cls: Class, fld)) =>
        val clsptr, fieldptr = fresh()

        val clsty = cls.classStruct: Type
        val index = sh"i32 ${fld.index + 1}"

        ll.inst(clsptr, sh"bitcast $obj to $clsty*")
        ll.inst(fieldptr,
                sh"getelementptr $clsty, $clsty* %$clsptr, i32 0, $index")
        ll.inst(name, sh"bitcast ${fld.ty}* %$fieldptr to i8*")

      case Op.Sizeof(ty) =>
        val elem = fresh()

        ll.inst(elem, sh"getelementptr $ty, $ty* null, i32 1")
        ll.inst(name, sh"ptrtoint $ty* %$elem to i64")

      case Op.Is(ty, value) =>
        genIs(name, ty, genVal(value))

      case Op.As(ty, value) =>
        genAs(name, ty, value)

      case Op.Method(sig, obj, meth) =>
        genMethod(name, sig, obj, meth)

      case Op.Module(module) =>
        val access = genJustGlobal(module tag "access")
        ll.invoke(name, sh"i8* () $access()")

      case Op.Copy(value) =>
        copy(name) = value

      case op =>
        unsupported(op)
    }
  }

  def genCall(name: Local, ty: Type, ptr: Val, args: Seq[Val]): Unit = {
    val Type.Function(_, resty) = ty
    val pointee = ptr match {
      case Val.Local(n, _) if copy.contains(n) =>
        return genCall(name, ty, copy(n), args)
      case Val.Global(n, _) =>
        genJustGlobal(n)
      case _ =>
        val cast = fresh()
        ll.inst(cast, sh"bitcast $ptr to $ty*")
        sh"%$cast"
    }
    val sig = sh"$ty $pointee(${r(args, sep = ", ")})"

    if (resty.isUnit) {
      ll.invoke(sig)
    } else if (resty.isNothing) {
      ll.invoke(sig)
      ll.unreachable()
      terminate()
    } else {
      ll.invoke(name, sig)
    }
  }

  def genIs(name: Local, ofty: Type, obj: Res): Unit = ofty match {
    case ClassRef(cls) =>
      val typtrptr, typtr = fresh()

      ll.inst(typtrptr, sh"bitcast $obj to $rtty**")
      ll.inst(typtr, sh"load $rtty*, $rtty** %$typtrptr")

      if (cls.range.length == 1) {
        val typtr1 = fresh()

        ll.inst(typtr1, sh"bitcast $rtty* %$typtr to i8*")
        ll.inst(name, sh"icmp eq i8* %$typtr1, ${typeConst(cls)}")

      } else {
        val idptr, id, ge, le = fresh()

        ll.inst(idptr, sh"getelementptr $rtty, $rtty* %$typtr, i32 0, i32 0")
        ll.inst(id, sh"load i32, i32* %$idptr")
        ll.inst(ge, sh"icmp sle i32 ${cls.range.start}, %$id")
        ll.inst(le, sh"icmp sle i32 %$id, ${cls.range.end}")
        ll.inst(name, sh"and i1 %$ge, %$le")
      }

    case TraitRef(trt) =>
      val typtrptr, typtr, idptr, id, boolptr = fresh()

      ll.inst(typtrptr, sh"bitcast $obj to $rtty**")
      ll.inst(typtr, sh"load $rtty*, $rtty** %$typtrptr")
      ll.inst(idptr, sh"getelementptr $rtty, $rtty* %$typtr, i32 0, i32 0")
      ll.inst(id, sh"load i32, i32* %$idptr")
      ll.inst(boolptr,
              sh"getelementptr $ity, $ival, i32 0, i32 %$id, i32 ${trt.id}")
      ll.inst(name, sh"load i1, i1* %$boolptr")
  }

  def genAs(name: Local, to: Type, v: Val): Unit = (to, v.ty) match {
    case (ty1, ty2) if ty1 == ty2 =>
      copy(name) = v

    case (_: Type.RefKind, _: Type.RefKind) =>
      copy(name) = v

    case (to @ Type.I(w1), Type.I(w2)) if w1 > w2 =>
      ll.inst(name, sh"sext $v to ${to: Type}")

    case (to @ Type.I(w1), Type.I(w2)) if w1 < w2 =>
      ll.inst(name, sh"trunc $v to ${to: Type}")

    case (to @ Type.I(_), Type.F(_)) =>
      ll.inst(name, sh"fptosi $v to ${to: Type}")

    case (to @ Type.F(_), Type.I(_)) =>
      ll.inst(name, sh"sitofp $v to ${to: Type}")

    case (to @ Type.F(w1), Type.F(w2)) if w1 > w2 =>
      ll.inst(name, sh"fpext $v to ${to: Type}")

    case (to @ Type.F(w1), Type.F(w2)) if w1 < w2 =>
      ll.inst(name, sh"fptrunc $v to ${to: Type}")

    case (Type.Ptr, _: Type.RefKind) =>
      ll.inst(name, sh"bitcast $v to i8*")

    case (to @ (_: Type.RefKind), Type.Ptr) =>
      ll.inst(name, sh"bitcast $v to ${to: Type}")
  }

  def genMethod(name: Local, sig: Type, obj: Val, meth: Global): Unit =
    meth match {
      case MethodRef(_: Class, meth) if meth.isStatic =>
        copy(name) = Val.Global(meth.name, Type.Ptr)

      case MethodRef(cls: Class, meth) if meth.isVirtual =>
        val typtrptr, typtr, methptrptr = fresh()

        val ty    = cls.typeStruct: Type
        val index = sh"i32 ${meth.vindex}"

        ll.inst(typtrptr, sh"bitcast $obj to $ty**")
        ll.inst(typtr, sh"load $ty*, $ty** %$typtrptr")
        ll.inst(methptrptr,
                sh"getelementptr $ty, $ty* %$typtr, i32 0, i32 2, $index")
        ll.inst(name, sh"load i8*, i8** %$methptrptr")

      case MethodRef(trt: Trait, meth) =>
        val typtrptr, typtr, idptr, id, methptrptr = fresh()

        val mid = sh"i32 ${meth.id}"

        ll.inst(typtrptr, sh"bitcast $obj to $rtty**")
        ll.inst(typtr, sh"load $rtty*, $rtty** %$typtrptr")
        ll.inst(idptr, sh"getelementptr $rtty, $rtty* %$typtr, i32 0, i32 0")
        ll.inst(id, sh"load i32, i32* %$idptr")
        ll.inst(methptrptr,
                sh"getelementptr $dty, $dval, i32 0, i32 %$id, $mid")
        ll.inst(name, sh"load i8*, i8** %$methptrptr")
    }

  implicit val genNext: Show[Next] = Show {
    case Next.Case(v, n) => sh"$v, label %$n"
    case next            => sh"label %${next.name}"
  }

  implicit def genConv: Show[Conv] = nir.Shows.showConv
}

object LLInstGen {
  val excrecty = sh"{ i8*, i32 }"

  val landingpad =
    sh"landingpad { i8*, i32 } catch i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN11scalanative16ExceptionWrapperE to i8*)"

  val typeid =
    sh"call i32 @llvm.eh.typeid.for(i8* bitcast ({ i8*, i8*, i8* }* @_ZTIN11scalanative16ExceptionWrapperE to i8*))"
}

private[codegen] final case object Terminate extends Exception
