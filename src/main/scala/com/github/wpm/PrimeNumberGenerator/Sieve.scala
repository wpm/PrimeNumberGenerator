package com.github.wpm.PrimeNumberGenerator

import collection.mutable

object Sieve {
  /**
   * Return a sequence of prime numbers from a sequence of integers using the Sieve of Eratosthenes.
   *
   * The integer sequence must consist of all integers greater than or equal to `p`, skipping values that are multiples
   * of primes less than `p`. The simplest input sequence is just 2,3,4,5...
   *
   * @param ns sequence of numbers
   * @return prime numbers in the sequence
   */
  def primes(ns: Stream[Int] = skip2357): Stream[Int] = {
    def primesRec(ns: Stream[Int], q: mutable.PriorityQueue[Stream[Int]]): Stream[Int] = {
      def smallestComposite = q.head.head
      val n = ns.head
      if (n >= smallestComposite) {
        while (n >= smallestComposite) q += q.dequeue().tail
        primesRec(ns.tail, q)
      }
      else n #:: primesRec(ns.tail, q += ns.map(n * _))
    }
    ns.head #:: primesRec(ns.tail, mutable.PriorityQueue(ns.map(ns.head * _))(MultipleOrdering))
  }

  /**
   * Prime multiple sequences are stored in the priority queue in the order of their smallest element.
   */
  object MultipleOrdering extends Ordering[Stream[Int]] {
    def compare(x: Stream[Int], y: Stream[Int]) = y.head.compare(x.head)
  }

  def wheel(start: Int, steps: Seq[Int]): Stream[Int] = {
    def wheelRec(n: Int, cycle: Stream[Int]): Stream[Int] = n #:: wheelRec(n + cycle.head, cycle.tail)
    wheelRec(start, Stream.from(0).flatMap(_ => steps))
  }

  /**
   * Integers from 5 on, skipping multiples of 2 and 3
   */
  val skip23 = wheel(5, Seq(2, 4))
  /**
   * Integers from 11 on, skipping multiples of 2, 3, 5, and 7
   */
  val skip2357 = wheel(11,
    Seq(2, 4, 2, 4, 6, 2, 6, 4, 2, 4, 6, 6, 2, 6, 4, 2, 6, 4, 6, 8, 4, 2, 4, 2, 4, 8, 6, 4, 6, 2, 4, 6, 2, 6, 6, 4, 2,
      4, 6, 2, 6, 4, 2, 4, 2, 10, 2, 10))

  def main(args: Array[String]) {
    val (fmt, ps) = if (args.isEmpty) ("%d: %d", primes())
    else {
      val n = args.head.toInt
      ("%" + n.toString.length + "d: %d", primes().take(n))
    }
    ps.zipWithIndex.foreach {
      case (p, i) => println(fmt.format(i + 1, p))
    }
  }
}
