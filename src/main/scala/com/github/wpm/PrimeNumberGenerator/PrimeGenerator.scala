package com.github.wpm.PrimeNumberGenerator


abstract class PrimeGenerator(candidates: Candidates, var sieve: Sieve) extends Iterator[Int] {
  def hasNext = true

  def next() = {
    while (sieve.contains(candidates.head)) sieve += candidates.next()
    val next = candidates.next()
    sieve += next
    next
  }

  override def toString() = sieve.toString
}

class MapGenerator extends PrimeGenerator(new Integers(), MapSieve())

object MapGenerator {
  def apply() = new MapGenerator()
}

class QueueGenerator(candidates: Candidates) extends PrimeGenerator(candidates, QueueSieve())

object QueueGenerator {
  def apply(candidates: Candidates = new Skip2357) = new QueueGenerator(candidates)
}
