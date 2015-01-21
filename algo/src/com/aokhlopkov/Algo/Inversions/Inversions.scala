package com.aokhlopkov.Algo.Inversions

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

object Inversions {
    def main(args: Array[String]) {
       val array = Source
         .fromFile("""/Users/Andrey/Downloads/IntegerArray.txt""")
         .getLines()
         .map(Integer.parseInt)
         .toList
      val t0 = System.nanoTime()
      //println(BruteForce(array))
      //println(BruteForceTR(array, 0))

      println(NumInversions(array))
      val t1 = System.nanoTime()
      println("Elapsed time: " + (t1 - t0) / 1000 + "ms")
    }

  def BruteForce(a: List[Int]): Int = {
    var count = 0
    for (i <- 0 until a.size - 1; j <- i until a.size) {
      if (a(i) > a(j)) count += 1
    }
    count
  }

  @tailrec
  def BruteForceTR(a: List[Int], sum: Int): Int = {
    a match {
      case head :: tail =>
        BruteForceTR(tail, sum + tail.count(_ < head))
      case _ => sum
    }
  }

  @tailrec
  def MergeAndCount(left: List[Int], right: List[Int], sum: Long, result: List[Int]): (Long, List[Int]) = {
    (left, right) match {
      case (leftHead :: leftTail, rightHead :: rightTail) if leftHead > rightHead =>
        MergeAndCount(left, rightTail, sum + left.size, rightHead :: result)
      case (leftHead :: leftTail, rightHead :: rightTail) =>
        MergeAndCount(leftTail, right, sum, leftHead :: result)
      case (leftHead :: leftTail, _) =>
        MergeAndCount(leftTail, right, sum, leftHead :: result)
      case (_, rightHead :: rightTail) =>
        MergeAndCount(left, rightTail, sum, rightHead :: result)
      case _ =>
        (sum, result.reverse)
    }
  }

  def NumInversions(a: List[Int]): (Long, List[Int]) = {
    a match {
      case Nil => (0, Nil)
      case head :: Nil => (0, head :: Nil)
      case _ =>
        val (left, right) = a.splitAt(a.size / 2)
        val (numLeft, sortedLeft) = NumInversions(left)
        val (numRight, sortedRight) = NumInversions(right)
        val (numCross, sorted) = MergeAndCount(sortedLeft, sortedRight, 0, Nil)
        (numLeft + numRight + numCross, sorted)
    }
  }
}
