#!/usr/bin/env scala

// ICPC Asia Japan 2026 Online First-Round Contest
// Problem B: Vending machines

import scala.io.StdIn

def solve(xs: Array[Int], d: Int): Int = {
  val v = -(d + 1)  // sentinel: the last vending machine
  xs.foldLeft((v, 0))((acc, x) => {
    val (v, nv) = acc  // pos-of-vending-machine, #-of-vending-machine
    if (v + d < x) (x + d, nv + 1) else (v, nv)
  })._2
}

def main(): Unit = {
    val Array(n, d) = inputIntArray()
    if (n != 0) {
      val xs = inputIntArray()
      assert(xs.length == n)

      println(solve(xs, d))
      main()
    }
}

def inputIntArray(): Array[Int] =
  StdIn.readLine().split(' ').map(_.toInt)

main()
