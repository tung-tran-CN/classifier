package com.condenast.ml.util

object Math {

  def sqrt(x: Double) = sqrtIter(1.0, x)

  private def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  private def improve(guess: Double, x: Double) = (guess + x / guess) / 2

  private def isGoodEnough(guess: Double, x: Double) = scala.math.abs(guess * guess - x) < 0.00000001
}