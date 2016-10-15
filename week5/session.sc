def last[T](xs: List[T]) : T = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => x
  case y :: ys => last(ys)
}

val a=List(1,2,3)
last(a)

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

//concat
//reverse

def removeAt[T](n: Int, xs: List[T]) = (xs take n) ::: (xs drop n + 1)

removeAt(1, List('a', 'b', 'c', 'd'))
// List(a, c, d)

//def merge(xs: List[Int], ys: List[Int]): List[Int] =
//  (xs, ys) match {
//  ???
//  }