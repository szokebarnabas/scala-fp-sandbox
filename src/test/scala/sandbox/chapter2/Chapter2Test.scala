package sandbox.chapter2

import org.scalatest.FlatSpec

class Chapter2Test extends FlatSpec {

  val chapter2 = new Chapter2
  "fact" should "return the factorial of the given value" in {
    assert(chapter2.fact(8) == 40320)
  }

  "fib" should "return the nth element of a fibonacci sequence" in {
    assert(chapter2.fib1(6) == 8)
  }

  "format result" should "calculate the abs value" in {
    def result = chapter2.formatResult("absolute value", -42, chapter2.abs)
    assert(result == "The absolute value of -42 is 42.")
  }

  it should "calculate the factorial" in {
    def result = chapter2.formatResult("factorial", 7, chapter2.fact)
    assert(result == "The factorial of 7 is 5040.")
  }

  it should "calculate the square root with an anonymous function" in {
    def result = chapter2.formatResult("sqrt", 25, x => Math.sqrt(x).toInt)
    assert(result == "The sqrt of 25 is 5.")
  }


  "is sorted" should "return true if the array is already sorted with the given sort function" in {
    val array: Array[Int] = Array(1,2,3,4,5,6,7,8)
    def sorted = chapter2.isSorted(array, (a: Int,b: Int) => a < b)
    assert(sorted)
  }

  it should "return false if the array is not sorted with the given sort function" in {
    val array: Array[Int] = Array(3,1,2,5,3,7,3)
    def sorted = chapter2.isSorted(array, (a: Int,b: Int) => a < b)
    assert(!sorted)
  }

  "sum" should "return the sum of the values" in {
    val array: Array[Int] = Array(2,4,5,6)
    assert(chapter2.sum(array) == 17)
  }
}
