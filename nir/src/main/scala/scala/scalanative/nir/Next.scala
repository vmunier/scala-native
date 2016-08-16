package scala.scalanative
package nir

sealed abstract class Next {
  def name: Local
}
object Next {
  final case class Label(name: Local, args: Seq[Val]) extends Next
  final case class Case(value: Val, name: Local)      extends Next
  final case class Catch(ty: Type, name: Local)       extends Next

  def apply(name: Local) = Label(name, Seq())
}
