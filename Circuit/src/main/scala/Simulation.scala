/**
  * Created by fang on 7/7/17.
  * The whole project can be implemented using listener pattern. Gates register to input wires,
  * output wires register to gates.
  */
package mysims;

abstract class Simulation {

  type Action = () => Unit

  case class WorkItem(time: Int, action: Action)

  private var curtime = 0
  def currentTime: Int = curtime

  // The agenda controls which wire of the circuit is "currently" updating. The wire will update
  // time and add additional items(corresponding to other unit using the wire as input) to the
  // agenda.
  //
  // Note that the update is aynchronous. If two items are of the same time, the processing result
  // will not be correct. It can be revised to process multiple tasks of the same time in a batch.
  //
  // Why List instead of other data structure with automatic ordering?
  // Scala provides two types of sorting data structure, TreeSet and TreeMap, however, they do not
  // support duplicated elements or keys, thus, duplicated tasks inserted will get lost. Using List
  // with customized sorting becomes the natural solution.
  private var agenda: List[WorkItem] = List()

  private def insert(ag: List[WorkItem],
                     item: WorkItem): List[WorkItem] = {

    if (ag.isEmpty || item.time < ag.head.time) item :: ag
    else ag.head :: insert(ag.tail, item)
  }

  // Schedule a new task with certain delay into agenda.
  def afterDelay(delay: Int)(block: => Unit) {
    val item = WorkItem(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  // Get next task in the agenda and run it.
  private def next() {
    (agenda: @unchecked) match {
      case item :: rest =>
        agenda = rest
        curtime = item.time
        item.action()
    }
  }

  // Insert a default 0-delay task and start the simulation.
  def run() {
    afterDelay(0) {
      println("*** simulation started, time = "+
        currentTime +" ***")
    }
    while (agenda.nonEmpty) next()
  }

  def init() {
    afterDelay(0) {}
    while (agenda.nonEmpty) next()
    curtime = 0
  }
}

abstract class BasicCircuitSimulation extends Simulation {

  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int

  abstract class Component

  class Wire extends Component {
    private var probeFlag: Boolean = false
    private var name: String = ""
    var signal: Boolean = false
    var listeners: List[Gate] = List[Gate]()

    def setSignal(s: Boolean): Unit = if (s != signal){
      signal = s
      postListeners()
      if (probeFlag) println(name + ":" + currentTime + ":" + signal)
    }
    def postListeners(): Unit = listeners.foreach(_.update())
    def addListener(g: Gate): Unit = listeners = g::listeners
    def setProbe(name: String): Unit = {
      this.name = name
      probeFlag = true
    }
  }

  // All subclasses of Circuit should have some wires as inputs, and some wires as outputs.
  abstract class Circuit extends Component

  abstract class Gate extends Circuit {
    // Register an initializing task to initialize gates with a stable status (output decided
    // by inputs).
    // When a new test circuit is created, we should run Simulation.init() (run the init tasks
    // created here) to get a stablized initial state.
    update()
    def update(): Unit  // Delayed update (on gate output).
  }

  case class Invertor(i: Wire, o: Wire) extends Gate {
    i.addListener(this)
    override def update(): Unit = afterDelay(InverterDelay){
      o.setSignal(!i.signal)
    }
  }
  case class And(i1: Wire, i2: Wire, o: Wire) extends Gate {
    i1.addListener(this)
    i2.addListener(this)
    override def update(): Unit = afterDelay(AndGateDelay){
      o.setSignal(i1.signal && i2.signal)
    }
  }
  case class Or(i1: Wire, i2: Wire, o: Wire) extends Gate {
    i1.addListener(this)
    i2.addListener(this)
    override def update(): Unit = afterDelay(OrGateDelay){
      o.setSignal(i1.signal || i2.signal)
    }
  }

  case class HalfAdder(a: Wire, b: Wire, s: Wire, c: Wire) extends Circuit {
    val d, e = new Wire
    val or = Or(a, b, d)
    val and1 = And(a, b, c)
    val inv = Invertor(c, e)
    val and2 = And(d, e, s)
  }

  case class FullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire) extends Circuit {
    val s, c1, c2 = new Wire
    val ha1 = HalfAdder(a, cin, s, c1)
    val ha2 = HalfAdder(b, s, sum, c2)
    val or = Or(c1, c2, cout)
  }
}

object MySimulation extends BasicCircuitSimulation {
  def InverterDelay = 1
  def AndGateDelay = 3
  def OrGateDelay = 5
}
