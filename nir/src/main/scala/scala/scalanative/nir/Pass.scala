package scala.scalanative
package nir

trait Pass {
  def onDefn(defn: Defn): Unit    = ()
  def onBlock(block: Block): Unit = ()
  def onInst(inst: Inst): Unit    = ()
  def onVal(value: Val): Unit     = ()
  def onType(ty: Type): Unit      = ()
  def onCf(cf: Cf): Unit          = ()
  def onNext(next: Next): Unit    = ()

  private def visitAssembly(assembly: Seq[Defn]): Unit =
    assembly.foreach(visitDefn)

  private def visitDefn(defn: Defn): Unit = {
    onDefn(defn)

    defn match {
      case defn @ Defn.Var(_, _, ty, value) =>
        visitType(ty)
        visitVal(value)
      case defn @ Defn.Const(_, _, ty, value) =>
        visitType(ty)
        visitVal(value)
      case defn @ Defn.Declare(_, _, ty) =>
        visitType(ty)
      case defn @ Defn.Define(_, _, ty, blocks) =>
        visitType(ty)
        blocks.foreach(visitBlock)
      case defn @ Defn.Struct(_, _, tys) =>
        tys.foreach(visitType)
      case _ =>
        ()
    }
  }

  private def visitBlock(block: Block): Unit = {
    onBlock(block)

    block.params.foreach(param => visitType(param.ty))
    block.insts.foreach(visitInst)
    visitCf(block.cf)
  }

  private def visitInst(inst: Inst): Unit = {
    onInst(inst)

    inst.op match {
      case Op.Call(ty, ptrv, argvs) =>
        visitType(ty)
        visitVal(ptrv)
        argvs.foreach(visitVal)
      case Op.Load(ty, ptrv) =>
        visitType(ty)
        visitVal(ptrv)
      case Op.Store(ty, ptrv, v) =>
        visitType(ty)
        visitVal(ptrv)
        visitVal(v)
      case Op.Elem(ty, ptrv, indexvs) =>
        visitType(ty)
        visitVal(ptrv)
        indexvs.foreach(visitVal)
      case Op.Extract(aggrv, indexes) =>
        visitVal(aggrv)
      case Op.Insert(aggrv, v, indexes) =>
        visitVal(aggrv)
        visitVal(v)
      case Op.Stackalloc(ty, v) =>
        visitType(ty)
        visitVal(v)
      case Op.Bin(bin, ty, lv, rv) =>
        visitType(ty)
        visitVal(lv)
        visitVal(rv)
      case Op.Comp(comp, ty, lv, rv) =>
        visitType(ty)
        visitVal(lv)
        visitVal(rv)
      case Op.Conv(conv, ty, v) =>
        visitType(ty)
        visitVal(v)
      case Op.Select(v1, v2, v3) =>
        visitVal(v1)
        visitVal(v2)
        visitVal(v3)

      case Op.Classalloc(n) =>
        ()
      case Op.Field(ty, v, n) =>
        visitType(ty)
        visitVal(v)
      case Op.Method(ty, v, n) =>
        visitType(ty)
        visitVal(v)
      case Op.Module(n) =>
        ()
      case Op.As(ty, v) =>
        visitType(ty)
        visitVal(v)
      case Op.Is(ty, v) =>
        visitType(ty)
        visitVal(v)
      case Op.Copy(v) =>
        visitVal(v)
      case Op.Sizeof(ty) =>
        visitType(ty)
      case Op.Closure(ty, fun, captures) =>
        visitType(ty)
        visitVal(fun)
        captures.foreach(visitVal)
    }
  }

  private def visitCf(cf: Cf): Unit = {
    onCf(cf)

    cf match {
      case Cf.Unreachable =>
        ()
      case Cf.Ret(v) =>
        visitVal(v)
      case Cf.Jump(next) =>
        visitNext(next)
      case Cf.If(v, thenp, elsep) =>
        visitVal(v)
        visitNext(thenp)
        visitNext(elsep)
      case Cf.Switch(v, default, cases) =>
        visitVal(v)
        visitNext(default)
        cases.foreach(visitNext)
      case Cf.Throw(v) =>
        visitVal(v)
      case Cf.Try(norm, cases) =>
        visitNext(norm)
        cases.map(visitNext)
    }
  }

  private def visitVal(value: Val): Unit = {
    onVal(value)

    value match {
      case Val.Zero(ty)          => visitType(ty)
      case Val.Undef(ty)         => visitType(ty)
      case Val.Struct(n, values) => values.foreach(visitVal)
      case Val.Array(ty, values) => visitType(ty); values.foreach(visitVal)
      case Val.Local(n, ty)      => visitType(ty)
      case Val.Global(n, ty)     => visitType(ty)
      case Val.Const(v)          => visitVal(v)
      case _                     => ()
    }
  }

  private def visitType(ty: Type): Unit = {
    onType(ty)

    ty match {
      case Type.Array(ty, n)      => visitType(ty)
      case Type.Function(tys, ty) => tys.foreach(visitType); visitType(ty)
      case Type.Struct(n, tys)    => tys.foreach(visitType)
      case _                      => ()
    }
  }

  private def visitNext(next: Next): Unit = {
    onNext(next)

    next match {
      case Next.Label(n, args) => args.foreach(visitVal)
      case Next.Case(v, n)     => visitVal(v)
      case Next.Catch(ty, n)   => visitType(ty)
    }
  }

  final def apply(assembly: Seq[Defn]): Unit = visitAssembly(assembly)
  final def apply(defn: Defn): Unit          = visitDefn(defn)
  final def apply(block: Block): Unit        = visitBlock(block)
  final def apply(inst: Inst): Unit          = visitInst(inst)
  final def apply(cf: Cf): Unit              = visitCf(cf)
  final def apply(next: Next): Unit          = visitNext(next)
  final def apply(value: Val): Unit          = visitVal(value)
  final def apply(ty: Type): Unit            = visitType(ty)
}
