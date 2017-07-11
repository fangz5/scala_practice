/**
  * Created by fang on 7/10/17.
  */
package mysims

import org.scalatest.FunSuite

class CircuitTest extends FunSuite{
  test("circuit1"){
    import mysims.MySimulation._

    val input1, input2, sum, carry = new Wire
    sum.name = "sum"
    carry.name = "carry"
    HalfAdder(input1, input2, sum, carry)
    //Invertor(input1, sum)
    //Invertor(input2, carry)
    MySimulation.init()
    input1.setSignal(true)
    MySimulation.run()
    input2.setSignal(true)
    MySimulation.run()
  }
}
