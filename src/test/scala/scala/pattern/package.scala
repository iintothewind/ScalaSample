package scala

package object pattern {

  // A sealed class cannot have any new subclasses added for matching except the ones in the same file
  sealed abstract class Expr

  case class Str(name: String) extends Expr

  case class Num(num: Double) extends Expr

  case class UnOp(operator: String, arg: Expr) extends Expr

  case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

  /**
   * implemented by pattern overlap
   * @param expr expression
   * @return
   */
  def simplify(expr: Expr): Expr = expr match {
    case UnOp("-", UnOp("-", e)) => simplify(e)
    case BinOp("+", e, Num(0)) => simplify(e)
    case BinOp("+", e, Str("")) => simplify(e)
    case BinOp("*", e, Num(1)) => simplify(e)
    case UnOp(op, e) => UnOp(op, simplify(e))
    case BinOp(op, left, right) => BinOp(op, simplify(left), simplify(right))
    case _ => expr
  }

  /* Compilation would show the below warning because the two possible cases are left out.
    Warning:(30, 38) match may not be exhaustive.
    It would fail on the following inputs: BinOp(_, _, _), UnOp(_, _)
  def describe(expr: Expr): String = expr match {
    case Num(_) => "a number"
    case Str(_) => "a string"
  }
  */
}
