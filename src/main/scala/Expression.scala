
/**
  * Created by Tonia on 20.10.2017.
  */
sealed trait Calculation
final case class Success(result: Double) extends adt.Calculation
final case class Failure(reason: String) extends adt.Calculation
trait Expression {
  def eval: adt.Calculation =
    this match {
      case Additional(l, r) =>
        l.eval match {
          case Failure(reason) => Failure(reason)
          case Success(r1) =>
            r.eval match {
              case Failure(reason) => Failure(reason)
              case Success(r2) => Success(r1 + r2)
            }
        }
      case Subtraction(l, r) =>
        l.eval match {
          case Failure(reason) => Failure(reason)
          case Success(r1) =>
            r.eval match {
              case Failure(reason) => Failure(reason)
              case Success(r2) => Success(r1 - r2)
            }
        }
      case Division(l, r) =>
        l.eval match {
          case Failure(reason) => Failure(reason)
          case Success(r1) =>
            r.eval match {
              case Failure(reason) => Failure(reason)
              case Success(r2) =>
                if (r2 == 0)
                  Failure("Division by zero")
                else
                  Success(r1 / r2)
            }
        }
      case SquareRoot(v) =>
        v.eval match {
          case Success(r) =>
            if (r < 0)
              Failure("Square root of negative number")
            else
              Success(Math.sqrt(r))
          case Failure(reason) => Failure(reason)
        }
      case Number(v) => Success(v)


    }
}
final case class Additional(left: Expression, right: Expression) extends Expression
final case class Subtraction (left: Expression, right: Expression) extends Expression
final case class Number (value: Double) extends Expression
final case class Division (left: Expression, right: Expression) extends Expression
final case class SquareRoot (value:Expression) extends Expression

assert(Additional(SquareRoot(Number(-1.0)), Number(2.0)).eval ==  Failure("Square root of negative number"))
assert(Additional(SquareRoot(Number(4.0)), Number(2.0)).eval == Success(4.0))
assert(Division(Number(4), Number(0)).eval == Failure("Division by zero"))

