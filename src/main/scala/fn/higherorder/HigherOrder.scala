/**
  * Created by Tonia on 29.10.2017.
  */
package fn.higherorder

//TODO: should be done
sealed trait IntList {
  def exists(func: Int => Boolean): Boolean =
    this match {
      case IntNil () => false
      case IntPair(h, t) => func(h)|| t.exists(func)
    }

  def filter(func: Int => Boolean): IntList =
    this match {
      case IntNil() => IntNil()
      case IntPair(h, t) => if (func(h))
        return IntPair (h, t.filter(func))
      else t.filter(func)

    }

  def find(func: Int => Boolean): IntList=
    this match {
      case IntNil () => IntNil ()
      case IntPair(h,t) => if (func(h))
        return this
        t.find(func)
    }
  }

final case class IntPair(head: Int, tail: IntList) extends IntList

case class IntNil() extends IntList

object Main extends App {
  val ints = IntPair(1, IntPair(3, IntPair(5, IntNil())))

  println(ints + """.exists(_ > 0)      == """ + ints.exists(_ > 0))
  println(ints + """.exists(_ < 0)      == """ + ints.exists(_ < 0))
  println(ints + """.exists(_ % 2 == 0) == """ + ints.exists(_ % 2 == 0))
  println(ints + """.exists(_ % 2 == 1) == """ + ints.exists(_ % 2 == 1))
}

