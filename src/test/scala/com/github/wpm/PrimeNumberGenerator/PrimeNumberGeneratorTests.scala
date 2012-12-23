package com.github.wpm.PrimeNumberGenerator

import org.scalatest.FlatSpec

class PrimeNumberGeneratorTests extends FlatSpec {
  private val take = 1000

  "The first " + take + " numbers from a wheel skipping 2,3,5 and 7" should "skip those factors" in {
    expect(None)(compareStreams(Sieve.skip2357, skip(2, 3, 5, 7), take))
  }

  "The first " + take + " integers" should "generate primes from 2 on" in {
    expect(None)(comparePrimeStream(Sieve.primes(Stream.from(2)), 2, take))
  }

  "The first " + take + " numbers from a wheel skipping 2,3,5, and 7" should "generate primes from 11 on" in {
    expect(None)(comparePrimeStream(Sieve.primes(), 11, take))
  }

  private def trialDivisionPrimes: Stream[Int] = {
    def primes(ns: Stream[Int]): Stream[Int] = ns.head #:: primes(ns.tail.filterNot(_ % ns.head == 0))
    primes(Stream.from(2))
  }

  private def skip(ns: Int*): Stream[Int] = Stream.from(2).filterNot(n => ns.exists(n % _ == 0))

  private def comparePrimeStream(primes: Stream[Int], start: Int, take: Int = 1000) = {
    compareStreams(primes, trialDivisionPrimes.dropWhile(_ < start), take)
  }

  private def compareStreams(actual: Stream[Int], expect: Stream[Int], take: Int) = {
    expect.zip(actual).take(take).find(p => p._1 != p._2)
  }
}
