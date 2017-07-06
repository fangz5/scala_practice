/**
  * Created by fang on 7/6/17.
  */
abstract class OperatorType
object Plus extends OperatorType
object Minus extends OperatorType
object Multiply extends OperatorType
object Divide extends OperatorType

sealed abstract class Expression {
  def layout: Layout
  override def toString: String = layout.toString
}

case class Number(n: Int) extends Expression {
  override def layout: Layout = new Layout(Array(n.toString))
}

case class Id(name: String) extends Expression {
  override def layout: Layout = new Layout(Array(name))
}

case class BinaryOperator(op: OperatorType, left: Expression, right: Expression)
  extends Expression {
  override def layout: Layout = {
    val opChar = op match {
      case Plus     => '+'
      case Minus    => '-'
      case Multiply => '*'
      case Divide   => '/'
    }

    if (op == Divide) {
      left.layout.topOn(right.layout)
    } else {
      left.layout.appendedWith(new Layout(Array(s" $opChar "))).appendedWith(right.layout)
    }
  }
}