package scala.scalanative
package compiler
package transform

import scala.collection.mutable
import scala.util.control.Breaks._
import util.unsupported
import nir._

/** Hoists all stack allocations to the entry basic block. */
class StackallocHoisting extends Transform {
  def apply(blocks: Seq[Block]) = {
    val allocs = mutable.UnrolledBuffer.empty[Inst]

    val head +: tail = blocks.map { block =>
      block.copy(insts = block.insts.flatMap {
        case inst @ Inst(_, alloc: Op.Stackalloc) =>
          allocs += inst
          Seq()
        case inst =>
          Seq(inst)
      })
    }

    head.copy(insts = allocs ++: head.insts) +: tail
  }
}

object StackallocHoisting {
  def apply(ctx: Ctx) = new StackallocHoisting
}
