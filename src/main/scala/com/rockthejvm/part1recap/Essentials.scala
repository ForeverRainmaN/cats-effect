package com.rockthejvm.part1recap

object Essentials {
  // values
  val aBoolean: Boolean = false

  // expressions are EVALUATED to a value
  val anIfExpression = if (2 > 3) "bigger" else "smaller"

  // instructions vs expression
  val theUnit = println("Hello, Scala") // Unit = "void" in other languages = ()

  // OOP
  class Animal

  class Cat extends Animal

  trait Carnivore {
    def eat(animal: Animal): Unit
  }

  // inheritance model: extend <= 1 class, but inherit from >= 0 traits
  class Crocodile extends Animal with Carnivore:
    override def eat(animal: Animal): Unit = println("Crunch!")

  // singleton
  object MySingleton // singleton pattern in one line


  // companions
  object Carnivore // companion object of the class Carnivore

  // generics
  class MyList[A]

  // method notation
  val three = 1 + 2
  val anotherThree = 1.+(2)

  // functional programming


  def main(args: Array[String]): Unit = {

  }
}
