package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1))) === List(Leaf('e',1), Leaf('t',2)))
  }
  test("makeOrderedLeafList for second frequency table") {
    assert(makeOrderedLeafList(List(('t', 3), ('e', 4), ('x', 2))) === List(Leaf('x',2), Leaf('t',3), Leaf('e',4)))
  }
  test("makeOrderedLeafList for third frequency table") {
    assert(makeOrderedLeafList(List(('a', 8), ('t', 3), ('e', 1), ('x', 2))) === List(Leaf('e',1), Leaf('x',2), Leaf('t',3), Leaf('a',8)))
  }

  test("combine of two leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3)))
  }
  test("combine of out of order two leaf list") {
    val leaflist = List(Leaf('e', 3), Leaf('t', 2))
    assert(combine(leaflist) === List(Fork(Leaf('t',2),Leaf('e',3),List('t', 'e'),5)))
  }
  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  test("combine of another leaf list") {
    val leaflist = List(Leaf('e', 3), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Leaf('x',4), Fork(Leaf('t',2),Leaf('e',3),List('t', 'e'),5)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
