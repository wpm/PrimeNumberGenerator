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

abstract class WheelCandidates(start: Int, steps: Array[Int]) extends Candidates {
  private var i = 0

  def next() = {
    val next = head
    head += steps(i)
    i = (i + 1) % steps.length
    next
  }

  var head = start
}

class Skip23 extends WheelCandidates(5, Array(2, 4))

class Skip2357 extends WheelCandidates(11,
  Array(2, 4, 2, 4, 6, 2, 6, 4, 2, 4, 6, 6, 2, 6, 4, 2, 6, 4, 6, 8, 4, 2, 4, 2, 4, 8, 6, 4, 6, 2, 4, 6, 2, 6, 6, 4, 2,
    4, 6, 2, 6, 4, 2, 4, 2, 10, 2, 10))
