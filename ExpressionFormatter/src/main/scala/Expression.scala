/**
  * Created by fang on 7/6/17.
  */
abstract case class OperatorType(sign: Char)
object Plus extends OperatorType('+')
object Minus extends OperatorType('-')
object Multiply extends OperatorType('*')
object Divide extends OperatorType('/')

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

case class BinaryOperator(op: OperatorType, l: Expression, r: Expression) extends Expression {
  override def layout: Layout = op match {
    case Divide => l.layout.topOn(r.layout)
    case _      => l.layout.appendedWith(new Layout(Array(s" ${op.sign} "))).appendedWith(r.layout)
  }
}