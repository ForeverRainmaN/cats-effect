package com.rockthejvm.part1recap

object ContextualAbstractionsScala3 {
  // given/using
  def increment(x: Int)(using amount: Int): Int = x + amount

  given defaultAmount: Int = 10

  val twelve = increment(2) // (10) automatically by the compiler

  def multiply(x: Int)(using factor: Int): Int = x * factor

  val aHundred = multiply(10)

  trait Combiner[A] {
    def combine(x: A, y: A): A

    def empty: A
  }


  def combineAll[A](values: List[A])(using combiner: Combiner[A]): A =
    values.foldLeft(combiner.empty)(combiner.combine)

  given intCombiner: Combiner[Int] with {
    override def combine(x: Int, y: Int): Int = x + y

    override def empty: Int = 0
  }

  val numbers = (1 to 10).toList
  val sum10 = combineAll(numbers) // Int combiner passed automatically

  // synthesize given instances
  given optionCombiner[T](using combiner: Combiner[T]): Combiner[Option[T]] with {
    override def empty: Option[T] = Some(combiner.empty)

    override def combine(x: Option[T], y: Option[T]): Option[T] = for {
      vx <- x
      vy <- y
    } yield combiner.combine(vx, vy)
  }

  val sumOptions: Option[Int] = combineAll(List(Some(1), None, Some(2)))

  // extension methods
  case class Person(name: String) {
    def greet(): String = s"Hi, my name is $name"
  }

  extension (name: String) def greet(): String = Person(name).greet()

  val alicesGreeting = "Alice".greet()

  // generic extension
  extension [T](list: List[T])
    def reduceAll(using combiner: Combiner[T]): T =
      list.foldLeft(combiner.empty)(combiner.combine)

  val numberSum = numbers.reduceAll


  def main(args: Array[String]): Unit = {

  }

}

object TypeClassesScala3 {

  case class Person(name: String, age: Int)
  // type classes scala 3

  // part 1 - Type class definition
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // part 2 - Type class instances

  given stringSerializer: JSONSerializer[String] with {
    def toJson(value: String): String = "\"" + value + "\""
  }

  given intSerializer: JSONSerializer[Int] with {
    def toJson(value: Int): String = value.toString
  }

  given personSerializer: JSONSerializer[Person] with {
    def toJson(person: Person): String =
      s"""
         |{"name": "${person.name}", "age": ${person.age}
         |""".stripMargin.trim
  }

  // part 3 - user-facing API
  def convert2Json[T](value: T)(using serializer: JSONSerializer[T]): String =
    serializer.toJson(value)

  def convertList2Json[T](list: List[T])(using serializer: JSONSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]")

  // part 4 - extension methods for types we support
  extension [T](value: T)
    def toJson(using serializer: JSONSerializer[T]): String =
      serializer.toJson(value)
}
