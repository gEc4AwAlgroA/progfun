object mytrue extends myBoolean {
  def ifThenElse[T](t: => T, e: => T): T = t
}

object myfalse extends myBoolean {
  def ifThenElse[T](t: => T, e: => T): T = e
}

abstract class myBoolean {
  def ifThenElse[T](t: => T, e: => T): T

  def &&(x: myBoolean): myBoolean = ifThenElse(x, myfalse)
  def ||(x: myBoolean): myBoolean = ifThenElse(mytrue, x)
  def unary_!(): myBoolean = ifThenElse(myfalse, mytrue)

  def ==(x: myBoolean): myBoolean = ifThenElse(x, x.unary_!)
  def !=(x: myBoolean): myBoolean = ifThenElse(x.unary_!, x)

  def <(x: myBoolean): myBoolean = ifThenElse(myfalse, x)
}

// Peano numbers
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor = new Succ(this)
  def +(that:Nat): Nat
  def -(that:Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new Error("0.predeccessor")
  def +(that:Nat) = that
  def -(that:Nat) = if (that.isZero) this else throw new Error("0.-")
}
class Succ(n:Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def +(that:Nat) = new Succ(n + that)
  def -(that:Nat) = if (that.isZero) this else n - that.predecessor
}

//trait List[T] {
//  def isEmpty: Boolean
//  def head: T
//  def tail: List[T]
//  def nth(n: Int): T = {
//    if (this.isEmpty) throw new IndexOutOfBoundsException
//    else if (n == 0) head
//    else tail.nth(n - 1)
//  }
//}
//class Cons[T](val head: T, val tail:List[T]) extends List[T] {
//  def isEmpty = false
//}
//class Nil[T] extends List[T] {
//  def isEmpty = true
//  def head = throw new NoSuchElementException("Nil.head")
//  def tail = throw new NoSuchElementException("Nil.tail")
//}
//
//object List {
//  def apply[T](): List[T] = new Nil
//  def apply[T](x1: T): List[T] = new Cons(x1, new Nil)
//  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
//}
//
//List()
//List(4)
//List(8,9)

//from week3
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

//val a: Array[NonEmpty] = Array(new NonEmpty(1, Empty, Empty))
//val b: Array[IntSet] = a
//b(0) = Empty
//val s: NonEmpty = a(0)

//Pattern Matching

trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1:Expr, e2:Expr) extends Expr
case class Prod(e1:Expr, e2:Expr) extends Expr
case class Var(str: String) extends Expr

def show (e:Expr): String = e match {
  case Number(x) => x.toString
  case Var(s) => s
  case Sum(x, y) => show(x) + " + " + show(y)
  case Prod(Number(x), Number(y)) => "x * y"
  case Prod(Number(x), Var(y)) => "x * " + show(Var(y))
  case Prod(Var(x), Var(y)) => show(Var(x)) + " * " + show(Var(y))
  case Prod(Var(x), Number(y)) => "x * y"
  case Prod(Sum(x,y), z) => "( " + show(Sum(x, y)) + " ) * " + show(z)
  case Prod(x, Sum(y,z)) => show(x) + " * ( " + show(Sum(y, z)) + ")"
  //I presume there's a much better way!
}

show(Var("d"))
show(Sum(Number(1), Number(13)))
show(Prod(Number(2),Number(4)))

show(Sum(Prod(Number(2), Var("a")), Var("b")))
show(Prod(Sum(Number(2), Var("c")), Var("d")))

//Lists

def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x <= y) x::xs else y :: insert(x, ys)
}

isort(List(1,3,5,4,2))
