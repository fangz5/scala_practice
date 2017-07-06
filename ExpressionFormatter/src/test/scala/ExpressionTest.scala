/**
  * Created by fang on 7/6/17.
  */
import org.scalatest.FunSuite

class ExpressionTest extends FunSuite{
  lazy val a = Id("a")
  lazy val b = BinaryOperator(Plus, Id("b"), Number(1))

  lazy val c = BinaryOperator(Divide, a, b)
  lazy val d = BinaryOperator(Divide, b, a)
  lazy val e = BinaryOperator(Plus, a, c)
  lazy val f = BinaryOperator(Divide, e, d)

  test("t1"){
    println("\nf:")
    println(f)
    assert(f.layout.height == 7 && f.layout.width == 9)
  }

}
