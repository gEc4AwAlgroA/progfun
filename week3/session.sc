abstract class IntSet {
  def contains(x: Int): Boolean
  def incl(x: Int): IntSet
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(other: IntSet) = other
  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  def union(other: IntSet) =
    ((left union right) union other) incl elem
  override def toString = "{" + left + elem + right + "}"
}

val t1 = new NonEmpty(1, Empty, Empty)
val t2 = new NonEmpty(2, Empty, Empty)
val t3 = new NonEmpty(3, Empty, Empty)
val t4 = new NonEmpty(4, Empty, Empty)
val ta = t1 union t2
val tb = ta union t3
val tc = tb union t4