package scala.scalanative
package compiler
package transform

import scala.collection.mutable
import nir._

trait Transform extends (Seq[Block] => Seq[Block]) {
  def apply(blocks: Seq[Block]): Seq[Block]
}

object Transform {
  def compose(transforms: Transform*): Transform = new Transform {
    def apply(blocks: Seq[Block]) = {
      var current = blocks
      transforms.foreach { tx =>
        current = tx(current)
      }
      current
    }
  }
}
