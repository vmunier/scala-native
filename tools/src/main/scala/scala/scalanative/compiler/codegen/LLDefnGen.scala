package scala.scalanative
package compiler
package codegen

import scala.collection.mutable
import compiler.analysis._
import ClassHierarchy._
import ClassHierarchyExtractors._
import util.{unsupported, unreachable, sh, Show}
import util.Show.{Sequence => s, Indent => i, Unindent => ui, Repeat => r, Newline => nl}
import nir._, Shows.brace

trait LLDefnGen { self: LLCodeGen =>
  import LLDefnGen._

  private lazy val gxxpersonality =
    sh"personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*)"

  /** Strips "extern." suffix from the given global. */
  protected def stripExtern(n: Global): Global = {
    val id = n.id
    assert(id.startsWith("extern."))
    Global.Top(id.substring(7))
  }

  lazy val dispatchTy =
    sh"[${world.classes.length} x [${world.traitMethods.length} x i8*]]"
  lazy val dispatch = sh"$dispatchTy* @${dispatchName: Global}"

  lazy val instanceTy =
    sh"[${world.classes.length} x [${world.traits.length} x i1]]"
  lazy val instance = sh"$instanceTy* @${instanceName: Global}"

  def genWorld(): Res = {
    import world._

    ll.start()
    genPrelude()
    classes.foreach(genNode)
    traits.foreach(genNode)
    structs.foreach(genNode)
    vars.foreach(genNode)
    consts.foreach(genNode)
    methods.foreach(genNode)
    genDispatchTable()
    genInstanceTable()
    genMain()
    ll.end()
  }

  def genMain() = {
    val mainTy =
      Type.Function(Seq(Type.Module(entry.top), ObjectArray), Type.Unit)
    val main   = Val.Global(entry, Type.Ptr)
    val argc   = Val.Local(fresh(), Type.I32)
    val argv   = Val.Local(fresh(), Type.Ptr)
    val module = Val.Local(fresh(), Type.Module(entry.top))
    val rt     = Val.Local(fresh(), Rt)
    val arr    = Val.Local(fresh(), ObjectArray)

    val blocks = Seq(
        Block(fresh(),
              Seq(argc, argv),
              Seq(Inst(rt.name, Op.Module(Rt.name)),
                  Inst(arr.name, Op.Call(initSig, init, Seq(rt, argc, argv))),
                  Inst(module.name, Op.Module(entry.top)),
                  Inst(Op.Call(mainTy, main, Seq(module, arr)))),
              Cf.Ret(Val.I32(0))))

    genMethod(Attrs.None, mainName, mainSig, blocks)
  }

  def genPrelude() = {
    ll.declare(void, scalanative_throw, Seq(i8_*), Seq())
    ll.declare(i8_*, scalanative_alloc, Seq(i8_*, i64), Seq())
    ll.declare(i32, llvm_eh_typeid_for, Seq(i8_*), Seq())
    ll.declare(i32, gxx_personality_v0, Seq(vararg), Seq())
    ll.declare(i8_*, cxa_begin_catch, Seq(i8_*), Seq())
    ll.declare(void, cxa_end_catch, Seq(), Seq())
    ll.global(scalanative_exception_wrapper,
              sh"{ i8*, i8*, i8* }",
              s(),
              isConst = true,
              isExtern = true)
  }

  def genNode(node: Node): Unit = {
    node match {
      case Var(attrs, name, ty, rhs) =>
        if (node.inWorld) {
          genGlobal(name, ty, rhs, isExtern = attrs.isExtern, isConst = false)
        }
      case Const(attrs, name, ty, rhs) =>
        genGlobal(name, ty, rhs, isExtern = attrs.isExtern, isConst = true)
      case Method(attrs, name, sig, blocks) =>
        genMethod(attrs, name, sig, blocks)
      case struct: Struct =>
        genRuntimeTypeInfo(struct)
        genStruct(struct.name, struct.tys)
      case cls: Class =>
        genRuntimeTypeInfo(cls)
        genClass(cls)
      case trt: Trait =>
        genRuntimeTypeInfo(trt)
      case _ =>
        unsupported(node)
    }
  }

  def genGlobal(name: nir.Global,
                ty: nir.Type,
                init: nir.Val,
                isExtern: Boolean,
                isConst: Boolean): Unit = {
    val stripped = if (isExtern) stripExtern(name) else name

    ll.global(name, ty, genJustVal(init), isConst, isExtern)
  }

  def genMethod(attrs: Attrs,
                name: Global,
                sig: Type,
                blocks: Seq[Block]): Unit = {
    val Type.Function(argtys, retty) = sig

    val retsvoid = retty.isUnit || retty.isNothing
    val llretty  = if (retsvoid) sh"void" else retty: Res
    val llname   = if (attrs.isExtern) stripExtern(name) else name
    val llargs =
      if (blocks.isEmpty) argtys: Seq[Res]
      else (blocks.head.params: Seq[Val]): Seq[Res]
    val llattrs = {
      val inline =
        if (attrs.inline == Attr.MayInline) Seq()
        else Seq((attrs.inline: Attr): Res)
      val personality = if (attrs.isExtern) Seq() else Seq(gxxpersonality: Res)
      inline ++ personality
    }

    if (blocks.isEmpty) {
      ll.declare(llretty, llname, argtys, llattrs)
    } else {
      ll.define(llretty,
                llname,
                llargs,
                llattrs,
                genBody(tx(blocks), retsvoid))
    }
  }

  def genStruct(name: Global, tys: Seq[Type]): Unit = {
    ll.struct(name, tys)
  }

  def genClass(cls: Class): Unit = {
    if (cls.isModule) {
      genModuleValue(cls)
      genModuleAccessor(cls)
    }
    genStruct(cls.classStruct.name, cls.classStruct.tys)
  }

  def genModuleValue(cls: Class): Unit = {
    val name = cls.name tag "value"
    val ty   = Type.Class(cls.name): Type
    val init = genJustVal(Val.Zero(ty))

    ll.global(name, ty, init, isConst = false, isExtern = false)
  }

  def genModuleAccessor(cls: Class): Unit = {
    val name = cls.name
    val body = {
      val entry, existing, initialize, self, cond, alloc = fresh()

      val value = genJustGlobal(name tag "value")

      ll.startBody()

      ll.block(entry)
      ll.inst(self, sh"load i8*, i8** $value")
      ll.inst(cond, sh"icmp ne i8* %$self, zeroinitializer")
      ll.branch(sh"i1 %$cond", existing, initialize)

      ll.block(existing)
      ll.ret(sh"i8* %$self")

      ll.block(initialize)
      genInst(alloc, Op.Classalloc(name))
      ll.inst(sh"store i8* %$alloc, i8** $value")
      if (world.nodes.contains(name tag "init")) {
        val init = genJustGlobal(name member "init")
        ll.invoke(sh"void (i8*) $init(i8* %$alloc)")
      }
      ll.ret(sh"i8* %$alloc")

      ll.endBody()
    }

    ll.define(i8_*, name tag "access", Seq(), Seq(), body)
  }

  def genRuntimeTypeInfo(node: Node): Unit = node match {
    case cls: Class =>
      genGlobal(cls.name tag "type",
                cls.typeStruct,
                cls.typeValue,
                isExtern = false,
                isConst = true)

    case _ =>
      val typeId  = Val.I32(node.id)
      val typeStr = Val.String(node.name.id)
      val typeVal = Val.Struct(nir.Rt.Type.name, Seq(typeId, typeStr))

      genGlobal(node.name tag "type",
                nir.Rt.Type,
                typeVal,
                isExtern = false,
                isConst = true)
  }

  def genDispatchTable(): Unit = {
    val columns = world.classes.sortBy(_.id).map { cls =>
      val row = Array.fill[Val](world.traitMethods.length)(Val.Null)
      cls.imap.foreach {
        case (meth, value) =>
          row(meth.id) = value
      }
      Val.Array(Type.Ptr, row)
    }
    val value =
      Val.Array(Type.Array(Type.Ptr, world.traitMethods.length), columns)

    genGlobal(dispatchName, value.ty, value, isExtern = false, isConst = true)
  }

  def genInstanceTable(): Unit = {
    val columns = world.classes.sortBy(_.id).map { cls =>
      val row = new Array[Boolean](world.traits.length)
      cls.alltraits.foreach { trt =>
        row(trt.id) = true
      }
      Val.Array(Type.Bool, row.map(Val.Bool))
    }
    val value = Val.Array(Type.Array(Type.Bool, world.traits.length), columns)

    genGlobal(instanceName, value.ty, value, isExtern = false, isConst = true)
  }

  implicit def genAttrSeq: Show[Seq[Attr]] = nir.Shows.showAttrSeq
  implicit def genAttr: Show[Attr]         = nir.Shows.showAttr
}

object LLDefnGen extends Depends {
  val void   = sh"void"
  val i32    = sh"i32"
  val i64    = sh"i64"
  val i8_*   = sh"i8*"
  val vararg = sh"..."

  val scalanative_throw  = Global.Top("scalanative_throw")
  val scalanative_alloc  = Global.Top("scalanative_alloc")
  val llvm_eh_typeid_for = Global.Top("llvm.eh.typeid.for")
  val gxx_personality_v0 = Global.Top("__gxx_personality_v0")
  val cxa_begin_catch    = Global.Top("__cxa_begin_catch")
  val cxa_end_catch      = Global.Top("__cxa_end_catch")
  val scalanative_exception_wrapper =
    Global.Top("_ZTIN11scalanative16ExceptionWrapperE")

  val dispatchName = Global.Top("__dispatch")
  val instanceName = Global.Top("__instance")

  val ObjectArray =
    Type.Class(Global.Top("scala.scalanative.runtime.ObjectArray"))

  val Rt       = Type.Module(Global.Top("scala.scalanative.runtime.package$"))
  val initName = Rt.name member "init_i32_ptr_class.ssnr.ObjectArray"
  val initSig  = Type.Function(Seq(Rt, Type.I32, Type.Ptr), ObjectArray)
  val init     = Val.Global(initName, initSig)

  val mainName = Global.Top("main")
  val mainSig  = Type.Function(Seq(Type.I32, Type.Ptr), Type.I32)

  val unitName  = Global.Top("scala.scalanative.runtime.BoxedUnit$")
  val unit      = Val.Global(unitName, Type.Ptr)
  val unitTy    = Type.Struct(unitName, Seq(Type.Ptr))
  val unitConst = Val.Global(unitName tag "type", Type.Ptr)
  val unitValue = Val.Struct(unitName tag "value", Seq(unitConst))

  override val depends = Seq(unitName, ObjectArray.name, Rt.name, init.name)
}
