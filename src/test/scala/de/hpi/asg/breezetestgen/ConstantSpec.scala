package de.hpi.asg.breezetestgen

import baseclasses.UnitTest
import de.hpi.asg.breezetestgen.domain.Constant

class ConstantSpec extends UnitTest {

  "A Constant" should "perform basic operations correctly" in {
    val c1 = Constant(7)
    val c2 = Constant(5)
    val truthy = Constant(1, 1)
    val falsy = Constant(0, 1)

    assert((c1 + c2) == Constant(12))
    assert((c1 - c2) == Constant(2))
    assert((c1 === c2) == falsy)
    assert((c1 !== c2) == truthy)
    assert((c1 < c2) == falsy)
    assert((c1 > c2) == truthy)
    assert((c1 <= c2) == falsy)
    assert((c1 >= c2) == truthy)
    assert((c1 & c2) == Constant(5))
    assert((c1 | c2) == Constant(7))
    assert((c1 ^ c2) == Constant(2))
    //assert(c1.! == Constant(-5)) // TODO: implement this for unsigned constants
  }

  it should "not exceed it maximum domain value" in {
    val c1 = Constant(1, 8)
    val c257 = Constant(257, 8)

    assert(c1.value == c257.value)

    assert(Constant(8, 3).value < 8)
    assert(Constant(8, 4, isSigned = true).value < 8)
  }
}
