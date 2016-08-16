package scala.scalanative
package compiler
package transform

import scala.collection.mutable
import analysis.ClassHierarchy.World
import nir._, Shows._
import util.sh

class DeadCodeElimination(implicit world: World) extends Transform {
  def apply(blocks: Seq[Block]) = {
    val usedef = analysis.UseDef(blocks)

    blocks.map { block =>
      val newInsts = block.insts.filter {
        case Inst(n, op) => usedef(n).alive
      }

      block.copy(insts = newInsts)
    }
  }
}

object DeadCodeElimination {
  def apply(ctx: Ctx) = new DeadCodeElimination()(ctx.world)
}
