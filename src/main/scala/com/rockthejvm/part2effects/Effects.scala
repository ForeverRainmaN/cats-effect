package com.rockthejvm.part2effects

import scala.concurrent.Future
import scala.io.StdIn

object Effects {

  // pure functional programming
  // substitution
  def combine(a: Int, b: Int): Int = a + b

  val five = combine(2, 3)
  val five_v2 = 2 + 3
  val five_v3 = 5

  // referential transparency = can replace an expression with it's value
  // as many times as we want without changing behaviour

  // example: print to the console
  val printSomething: Unit = println("Cats Effect")
  val printSomething_v2: Unit = () // not the same

  // example: change a variable
  var anInt = 0
  val changingVar: Unit = anInt += 1
  val changingVar_v2: Unit = () // not the same

  // side effects are inevitable for useful programs

  // effect
  /*
    Desires:
      - type signature describes the kind of calculation that will be performed
      - type signature describes the VALUE that will be calculated
      - when side effects are needed, effect construction is separate from effect execution
   */

  /*
      example: Option is an effect type
      - describes possibly absent value
      - computes a value of type A, if it exists
      - side effects are not needed
   */
  val anOption: Option[Int] = Option(42)
  /*
    example: Future is NOT an effect type
    - describes an async computation
    - computes a value of type A, if it's successful
    - side effect is required (allocating/scheduling a thread), execution is NOT separate from construction
   */

  import scala.concurrent.ExecutionContext.Implicits.global

  val future: Future[Int] = Future[Int](42)

  /*
    example: MyIO data type from the Monads lesson - it IS an effect type
    - describes any computation that might produce side effects
    - calculates a value of type A if it's successful
    - side effects are required for the evaluation of () => A
      creation of MyIO does NOT produce the side effects on construction
   */
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIO: MyIO[Int] = MyIO(() => {
    println("I am writing something...")
    42
  })

  val clock: MyIO[Long] = MyIO(() => System.currentTimeMillis())

  def measure[A](computation: MyIO[A]): MyIO[Long] =
    for {
      startTime <- clock
      _ <- computation
      finishTime <- clock
    } yield finishTime - startTime
  /*
     clock.flatMap(startTime => computation.flatMap(_ => clock.map(finishTime => finishTime - startTime)))

      clock.map(finishTime => finishTime - startTime) = MyIO(() => System.currentTimeMillis() - startTime)
      => clock.flatMap(startTime => computation.flatMap(_ => MyIO(() => System.currentTimeMillis() - startTime)))

      computation.flatMap(lambda) = MyIO(() => Lambda(___COM___).unsafeRun())
                                  = MyIO(() => System.currentTimeMillis() - startTime)).unsafeRun())
                                  = MyIO(() => System.currentTimeMillis() - startTime)
   */

  def printToConsole: MyIO[Unit] = MyIO(() => println("I love Scala!"))

  def readInputFromConsoleIO: MyIO[String] = MyIO(() => StdIn.readLine())

  def main(args: Array[String]): Unit = {
    println(clock.unsafeRun())
    println(measure(MyIO(() => List(1, 2, 3, 4, 5, 6, 7).map(x => x * 2))).unsafeRun())
    printToConsole.unsafeRun()
    println(readInputFromConsoleIO.unsafeRun())
  }
}
