package com.github.wpm.PrimeNumberGenerator

import collection.mutable

object Sieve {
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

  object MultipleOrdering extends Ordering[Stream[Int]] {
    def compare(x: Stream[Int], y: Stream[Int]) = y.head.compare(x.head)
  }

  def wheel(start: Int, steps: Seq[Int]): Stream[Int] = {
    def wheelRec(n: Int, cycle: Stream[Int]): Stream[Int] = n #:: wheelRec(n + cycle.head, cycle.tail)
    wheelRec(start, Stream.from(0).flatMap(_ => steps))
  }

  val skip23 = wheel(5, Seq(2, 4))
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
