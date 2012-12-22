package com.github.wpm.PrimeNumberGenerator

class MapSieve private(s: Map[Int, List[Multiples]] = Map()) {
  def contains(n: Int) = s.contains(n)

  def +(implicit ms: Multiples): MapSieve = {
    new MapSieve(if (contains(ms.n)) (s - ms.n) ++ s(ms.n).map {
      m =>
        m.next()
        mapEntry(s, m)
    }
    else s + mapEntry(s, ms))
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
