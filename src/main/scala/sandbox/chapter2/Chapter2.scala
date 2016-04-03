package sandbox.chapter2

import scala.annotation.tailrec

class Chapter2 extends App {

  def fact(num: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = if (n <= 0) acc else go(n - 1, n * acc)
    go(num, 1)
  }

  def fib1(n: Int): Int = n match {
    case 0 | 1 => n
    case _ => fib1(n - 1) + fib1(n - 2)
  }

  def abs(value: Int): Int = {
    if (value < 0) -value else value
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }


  def isSorted[A](arr: Array[A], f: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(n: Int): Boolean = {
      if (n >= arr.length - 1) true
      else if (!f(arr(n), arr(n + 1))) false
      else go(n + 1)
    }
    go(0)
  }

  def sum(array: Array[Int]) : Int = {
    @tailrec
    def go(n: Int, sum: Int) : Int = {
      if (n > array.length - 1) sum
      else go(n + 1, sum + array(n))
    }

    go(0, 0)
  }
}