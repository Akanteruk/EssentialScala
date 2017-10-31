/**
  * Created by Tonia on 20.10.2017.
  */
sealed trait TreePolimor {
  def sum: Int
  def double: TreePolimor
}
final case class LeafP (val elt: Int) extends TreePolimor {
  def sum: Int = elt
  def double: TreePolimor = LeafP (2*elt)
}
final case class NodeP (val rt: TreePolimor, val lt: TreePolimor) extends TreePolimor {
  def sum: Int = rt.sum + lt.sum
  def double: TreePolimor = NodeP (rt.double, lt.double)
}
