package scala.scalanative
package compiler

import nir._

trait Depends {
  def depends: Seq[Global] = Seq()
  def injects: Seq[Defn]   = Seq()
}
