package com.rockthejvm.part1recap

import scala.language.implicitConversions

object ContextualAbstractionsScala2 {
  case class Person(name: String) {
    def greet(): String = s"Hi, my name is $name"
  }

  implicit class ImpersonableString(name: String) {
    def greet(): String =
      Person(name).greet()
  }

  // extension method
  val greeting = "Peter".greet() // new ImpersonableString("Peter").greet()

  // example: scala.concurent.duratiion

  import scala.concurrent.duration.*

  val oneSecond = 1.second

  // implicit arguments

  def increment(x: Int)(implicit amount: Int) = x + amount

  implicit val defaultAmount: Int = 10
  val twelve = increment(2) // implicit argument 10 passed by the compiler


  def multiply(x: Int)(implicit factor: Int) = x * factor

  val aHunderd = multiply(10) // same implicit argument passed by the compiler

  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  def convertToJson[T](value: T)(implicit serializer: JSONSerializer[T]): String = serializer.toJson(value)

  implicit val PersonSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
    override def toJson(value: Person): String = "{\"name\" : \"" + value.name + "\"}"
  }

  val davidsJson = convertToJson(Person("David")) // implicit serializer passed here

  // implicit def
  implicit def createListSerializer[T](implicit serializer: JSONSerializer[T]): JSONSerializer[List[T]] =
    new JSONSerializer[List[T]] {
      override def toJson(list: List[T]): String = s"[${list.map(serializer.toJson).mkString(",")}]"
    }

  val personsJson = convertToJson(List(Person("Alice"), Person("Bob")))

  // implicit conversions (not recommended)
  case class Cat(name: String) {
    def meow(): String = s"$name is meowing"
  }

  implicit def string2Cat(name: String): Cat = Cat(name)

  val aCat: Cat = "Garfield"
  val garfieldMeowing = "Garfield".meow()
}

object TypeClassesScala2 {
  case class Person(name: String, age: Int)

  // part 1 - Type class definition
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // part 2 - type class instances
  implicit object StringSerializer extends JSONSerializer[String] {
    override def toJson(value: String): String = "\"" + value + "\""
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    override def toJson(value: Int): String = value.toString
  }

  // part 3 - offer some api
  def convertToJson[T](value: T)(implicit serializer: JSONSerializer[T]): String =
    serializer.toJson(value)

  def convertListToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]")

  // part 4 - extension methods
  object JsonSyntax {
    implicit class JSONSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }
  }

  def main(args: Array[String]): Unit = {
    import JsonSyntax.*

    val json = 25.toJson // available because we have implicit IntSerializer in scope
  }
}
