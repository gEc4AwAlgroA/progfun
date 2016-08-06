import scala.annotation.tailrec
import math.abs

def sqrt(x: Double) = {
  def isGoodEnough(guess: Double) =
    abs(guess * guess - x) < x * 1e-8
  def improve(guess: Double) =
    (guess + x / guess) / 2
  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  sqrtIter(1.0)
}

sqrt(4)
sqrt(2)
sqrt(1e-8)
sqrt(1e80)

def factorial(n: Int): BigInt = {
  @tailrec
  def factIter(n: Int, temp: BigInt): BigInt =
    if (n == 0) temp
    else factIter(n - 1, n * temp)
  factIter(n, 1)
}

factorial(6)