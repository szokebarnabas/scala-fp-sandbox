package sandbox

class MyBox[T](val value: T) {
  //functor: (A => B) => (MyBox[A] => MyBox[B])
  def map[A, B](rawFunction: A => B): (MyBox[A] => MyBox[B]) = (a: MyBox[A]) => new MyBox(rawFunction(a.value))

  //monad: (A => MyBox[B]) => (MyBox[A] => MyBox[B])
  def flatMap[A, B](func: A => MyBox[B]): MyBox[A] => MyBox[B] = (a: MyBox[A]) => func(a.value);

  //applicative: MyBox[A=>B] => MyBox[A] => MyBox[B]
  def apply[A,B](func: MyBox[A=>B]) : MyBox[A] => MyBox[B] = (a: MyBox[A]) => new MyBox(func.value(a.value));
}

object Test extends App {

  //functor: (A => B) => (MyBox[A] => MyBox[B])
  def map[A, B](rawFunction: A => B): (MyBox[A] => MyBox[B]) = (a: MyBox[A]) => new MyBox(rawFunction(a.value))
  def rawLengthOf(a: String): Int = a.length
  val boxedString: MyBox[String] = new MyBox("Hello")
  val transformedLenghtOf = map(rawLengthOf)
  val result1 = transformedLenghtOf(boxedString)


  //monad: (A => MyBox[B]) => (MyBox[A] => MyBox[B])
  def sizeOf(value: String): MyBox[Int] = new MyBox(value.length)

  def flatMap[A, B](func: A => MyBox[B]): MyBox[A] => MyBox[B] = (a: MyBox[A]) => func(a.value);

  val transformedLenghtOf2 = flatMap(sizeOf)
  val result2: MyBox[Int] = transformedLenghtOf2(boxedString)

  //applicative: MyBox[A=>B] => MyBox[A] => MyBox[B]
  def boxedRawLengthOf = new MyBox(rawLengthOf _)
  def apply[A,B](func: MyBox[A=>B]) : MyBox[A] => MyBox[B] = (a: MyBox[A]) => new MyBox(func.value(a.value));

}
