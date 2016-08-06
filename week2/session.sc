import math.abs

def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}

sum(x => x * x)(1, 10)

def product(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc * f(a))
  }
  loop(a, 1)
}

product(x => x)(1, 5)

def factorial2(n: Int) = product(x => x)(1, n)
factorial2(5)

def sumprod(f: Int => Int, combine: (Int, Int) => Int, init: Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, combine(acc, f(a)))
  }
  loop(a, init)
}

sumprod(x => x, (x, y) => x + y, 0)(1, 5000)

def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  val tolerance = 1e-6
  def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / x) < tolerance
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}

def sqrt2(x: Double) = fixedPoint(averageDamp(y => x / y))(1)
sqrt2(2)
sqrt2(14)


class Rational(x: Int, y: Int) {
  require(y!=0, "denominator must be non-zero")

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)

  def numer = x/g
  def denom = y/g

  def less(that: Rational) = this.numer * that.denom < that.numer * this.denom
  def max(that: Rational) = if (this.less(that)) that else this
  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def neg = new Rational(-numer, denom)

  def sub(that: Rational) = add(that.neg)

  override def toString = numer + "/" + denom
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x.add(y)
x.neg

x.sub(y)
x.sub(y).sub(z)
y.add(y)
y.less(x)
x.max(y).max(z)

x add z
