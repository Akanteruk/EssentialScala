
/**
  * Created by Tonia on 20.10.2017.
  */
sealed trait Tree
final case class Node (val r: Tree, val l: Tree) extends Tree
final case class Leaf (val el: Int) extends Tree

//Why there are used val as parammetr?
object TreeSum {
  def sum(tree: Tree): Int =
    tree match {
      case Leaf(el) => el
      case Node(r, l) => sum(r) + sum(l)
    }
}

object TreeDouble {
  def double(tree: Tree): Tree =
    tree match {
      case Leaf(el) => Leaf(2 * el)
      case Node(r, l) => Node(double(r), double(l))
    }
}


