/**
  * Created by fang on 7/5/17.
  */
import org.scalatest.FunSuite

class LayoutTest extends FunSuite{
  lazy val a = Array("a")
  lazy val b = Array("b+1")

  lazy val l1 = new Layout(a)
  lazy val l2 = new Layout(b)

  lazy val l3: Layout = l1.topOn(l2)
  lazy val l4: Layout = l2.topOn(l1)

  lazy val l5: Layout = l1.appendedWith(new Layout(Array(" + "))).appendedWith(l3)

  lazy val l6: Layout = l5.topOn(l4)

  def verify(name: String, l: Layout, h: Int, w: Int): Unit = {
    println("\n" + name + ":")
    println(l)
    assert(l.height == h && l.width == w)
  }

  test("t1"){
    verify("l1", l1, 1, 1)
  }

  test("t2"){
    verify("l2", l2, 1, 3)
  }

  test("t3"){
    verify("l3", l3, 3, 3)
  }

  test("t5"){
    verify("l5", l5, 3, 7)
  }

  test("t6"){
    verify("l6", l6, 7, 7)
  }

}
