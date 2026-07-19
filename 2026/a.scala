#!/usr/bin/env scala

// ICPC Asia Japan 2026 Online First-Round Contest
// Problem A: Find the strongest card

import scala.io.StdIn

def main(): Unit = {
    val n = StdIn.readInt()
    if (n != 0) {
      val cs: Array[Int] = StdIn.readLine().split(' ').map(_.toInt)
      assert(cs.length == n)

      val m = cs.maxBy(c => (c + 10) % 13)
      println(m)

      main()
    }
}

main()
