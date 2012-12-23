package com.github.wpm.PrimeNumberGenerator

// TODO Multiples doesn't have to be a buffered iterator if it has an advance method. Have to change unit tests.
/**
 * An infinite list of the multiples of a number starting at its square.
 */
class Multiples(val n: Int) extends BufferedIterator[Int] {
  val hasNext = true

  var head = n * n

  def next() = {
    val next: Int = head
    head += n
    next
  }

  def advance: Multiples = {
    next()
    this
  }

  override def toString() = "%d:%s...".format(n, (0 to 2).map(head + n * _).mkString(","))
}

object Multiples {
  def apply(n: Int) = new Multiples(n)
}