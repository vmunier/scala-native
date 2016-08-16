package scala.scalanative
package compiler
package analysis

import ClassHierarchy._
import nir._

object ClassHierarchyExtractors {
  trait Extractor[T] {
    def unapply(ty: nir.Type)(implicit world: World): Option[T] = ty match {
      case ty: Type.Named => unapply(ty.name)
      case _              => None
    }
    def unapply(name: Global)(implicit world: World): Option[T]
  }

  object Ref extends Extractor[Node] {
    def unapply(name: Global)(implicit world: World): Option[Node] =
      world.nodes.get(name)
  }

  object ScopeRef extends Extractor[Scope] {
    def unapply(name: Global)(implicit world: World): Option[Scope] =
      world.nodes.get(name).collect {
        case node: Scope => node
      }
  }

  object StructRef extends Extractor[Struct] {
    def unapply(name: Global)(implicit world: World): Option[Struct] =
      world.nodes.get(name).collect {
        case node: Struct => node
      }
  }

  object ClassRef extends Extractor[Class] {
    def unapply(name: Global)(implicit world: World): Option[Class] =
      world.nodes.get(name).collect {
        case node: Class => node
      }
  }

  object TraitRef extends Extractor[Trait] {
    def unapply(name: Global)(implicit world: World): Option[Trait] =
      world.nodes.get(name).collect {
        case node: Trait => node
      }
  }

  object MethodRef extends Extractor[(Scope, Method)] {
    def unapply(name: Global)(implicit world: World): Option[(Scope, Method)] =
      world.nodes.get(name).collect {
        case node: Method => (node.in, node)
      }
  }

  object VarRef extends Extractor[(Scope, Var)] {
    def unapply(name: Global)(implicit world: World): Option[(Scope, Var)] =
      world.nodes.get(name).collect {
        case node: Var => (node.in, node)
      }
  }

  object ConstRef extends Extractor[(Scope, Const)] {
    def unapply(name: Global)(implicit world: World): Option[(Scope, Const)] =
      world.nodes.get(name).collect {
        case node: Const => (node.in, node)
      }
  }
}
