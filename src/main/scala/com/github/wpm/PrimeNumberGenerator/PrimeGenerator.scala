package com.github.wpm.PrimeNumberGenerator


abstract class PrimeGenerator(candidates: Candidates, var sieve: MapSieve) extends Iterator[Int] {
  def hasNext = true

  def next() = {
    while (sieve.contains(candidates.head)) sieve += candidates.next()
    val next = candidates.next()
    sieve += next
    next
  }

  override def toString() = sieve.toString
}

class MapGenerator(candidates: Candidates) extends PrimeGenerator(candidates, MapSieve())

object MapGenerator {
  def apply(candidates: Candidates = new Integers()) = new MapGenerator(candidates)
}

