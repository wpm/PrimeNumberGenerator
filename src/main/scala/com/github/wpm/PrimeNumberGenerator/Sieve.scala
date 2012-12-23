package com.github.wpm.PrimeNumberGenerator

abstract class Sieve {
  def contains(n: Int): Boolean

  def +(n:Int): Sieve
}
