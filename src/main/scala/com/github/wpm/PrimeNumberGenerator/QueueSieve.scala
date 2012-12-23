package com.github.wpm.PrimeNumberGenerator

import collection.mutable

class QueueSieve private(q: mutable.PriorityQueue[Multiples] = mutable.PriorityQueue()(MultiplesOrder)) extends Sieve {
  def contains(n: Int) = !q.isEmpty && q.head.head <= n

  def +(n: Int): QueueSieve = {
    if (q.isEmpty || n < q.head.head) q += Multiples(n)
    else while (n >= q.head.head) {
      val ms = q.dequeue()
      ms.next()
      q += ms
    }
    this
  }

  override def toString = if (q.isEmpty) "Empty" else q.mkString("\n")
}

object MultiplesOrder extends Ordering[Multiples] {
  def compare(x: Multiples, y: Multiples) = y.head.compare(x.head)
}

object QueueSieve {
  def apply() = new QueueSieve()
}