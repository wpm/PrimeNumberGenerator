package com.github.wpm.PrimeNumberGenerator

/**
 * An infinite sequence of numbers to test for primality
 */
abstract class Candidates extends BufferedIterator[Int] {
  def hasNext = true
}

class Integers extends Candidates {
  def next() = {
    val next = head
    head += 1
    next
  }

  var head = 2
}

class OddIntegers extends Candidates {
  def next() = {
    val next = head
    head += 2
    next
  }

  var head = 3
}
