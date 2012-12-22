package com.github.wpm.PrimeNumberGenerator

import org.scalatest.FlatSpec

class PrimeNumberGeneratorTests extends FlatSpec {
  behavior of "Multiples"

  they should "start at the square of a number" in {
    expect(4)(Multiples(2).head)
    expect(9)(Multiples(3).head)
    expect(25)(Multiples(5).head)
  }

  they should "be an increasing sequence of multiples their base" in {
    expect(List(4, 6, 8, 10))(Multiples(2).take(4).toList)
    expect(List(9, 12, 15, 18))(Multiples(3).take(4).toList)
    expect(List(25, 30, 35, 40))(Multiples(5).take(4).toList)
  }

  behavior of "Candidates"

  they should "be increasing sequences, skipping appropriate factors" in {
    expect(List(2, 3, 4, 5))(new Integers().take(4).toList)
    expect(List(3, 5, 7, 9))(new OddIntegers().take(4).toList)
  }

  behavior of "Sieve"

  it should "always contain the next composite number and not the next prime" in {
    expect(false)(MapSieve().contains(2))
    expect(false)((MapSieve() + 2).contains(3))
    expect(true)((MapSieve() + 2 + 3).contains(4))
    expect(false)((MapSieve() + 2 + 3 + 4).contains(5))
  }

  private val n = 1000

  behavior of "MapGenerator"

  it should "produce the same first " + n + " primes as trial division starting at 2" in {
    expect(None)(MapGenerator().zip(trialDivisionPrimes).take(n).find(p => p._1 != p._2))
  }

  private def trialDivisionPrimes: Iterator[Int] = {
    def primes(ns: Iterator[Int]): Iterator[Int] = {
      val n = ns.next()
      Iterator(n) ++ primes(ns.filterNot(_ % n == 0))
    }
    primes(Iterator.from(2))
  }
}