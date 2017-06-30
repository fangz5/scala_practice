/**
 * private[class] is not a very safe way to enclose things.
 * Below is an example how the "privacy" is compromised. Use
 * private[this] if you can. The conclusion can be also applied
 * to private[package].
 */

class P {
  private[P] class A {override def toString(): String = "A"  }
  val a: A = new A
}

println((new P).a)
