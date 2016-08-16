package scala.scalanative
package nir

final case class Local(scope: String, id: Int) {
  def tag(s: String) = new Local(scope + "." + s, id)
}
