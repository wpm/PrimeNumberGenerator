package com.github.wpm.PrimeNumberGenerator

class MapSieve private(s: Map[Int, List[Multiples]] = Map()) extends Sieve {
  def contains(n: Int) = s.contains(n)

  def +(n: Int): MapSieve = {
    new MapSieve(if (contains(n)) (s - n) ++ s(n).map(ms => mapEntry(s, ms.advance))
    else s + mapEntry(s, Multiples(n)))
  }

  private def mapEntry(m: Map[Int, List[Multiples]], ms: Multiples): (Int, List[Multiples]) = {
    (ms.head -> (ms :: m.getOrElse(ms.head, List.empty[Multiples])))
  }

  override def toString = {
    if (s.isEmpty) "Empty"
    else s.keys.toSeq.sorted.map(k => "%d\t\t%s".format(k, s(k).mkString(", "))).mkString("\n")
  }
}

object MapSieve {
  def apply() = new MapSieve
}
