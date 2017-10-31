package adt

/**
  * Created by Tonia on 29.10.2017.
  */
sealed trait Calc
// TODO: Implement Calc
final case class Success(result: Double) extends Calc
final case class Failure(reason: String) extends Calc

sealed trait Expressiion {
  def eval: Calc =
    this match {
      case Addition(l, r) =>
        l.eval match {
          case Failure(reason) => Failure(reason)
          case Success(r1) =>
            r.eval match {
              case Failure(reason) => Failure(reason)
              case Success(r2) => Success(r1 + r2)
            }
}
      case Number (n) => n
      case Addition (l, r) => l.eval + r.eval
      case Subctraction (l,r) => l.eval - r.eval
      case Division (l, r) => l.eval / r.eval
      case SquareRoot (ex) => Math.sqrt(ex.eval)

}
final case class Addition(left: Expressiion, right: Expressiion) extends Expressiion
final case class Subctraction (left: Expressiion, right: Expressiion) extends Expressiion
final case class Number (n: Double) extends Expressiion
final case class Division (left:Expressiion,right: Expressiion) extends  Expressiion
final  case class  SquareRoot (ex:Expressiion) extends Expressiion


object Interpreter {
  // TODO: Implement a simple interpreter for Calc
}

object Main extends App {
