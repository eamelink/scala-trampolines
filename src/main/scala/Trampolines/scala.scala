package Trampolines

import org.openjdk.jmh.annotations.{ Benchmark, Scope, State }
import scala.annotation.tailrec

@State(Scope.Benchmark)
class StackRunner {
  def even(i: Int): Boolean = i match {
    case 0 => true
    case _ => odd(i - 1)
  }

  def odd(i: Int): Boolean = i match {
    case 0 => false
    case _ => even(i - 1)
  }

  @Benchmark
  def bench() =
    if (even(3000)) "Yeah" else "Neh"

}

@State(Scope.Benchmark)
class SpecializedTrampolineRunner {
  sealed trait EvenOdd
  case class Done(result: Boolean) extends EvenOdd
  case class Even(value: Int) extends EvenOdd
  case class Odd(value: Int) extends EvenOdd

  def even(i: Int): EvenOdd = i match {
    case 0 => Done(true)
    case _ => Odd(i - 1)
  }

  def odd(i: Int): EvenOdd = i match {
    case 0 => Done(false)
    case _ => Even(i - 1)
  }

  // Using Scala's self recursive tail call optimization
  @tailrec final def run(evenOdd: EvenOdd): Boolean = evenOdd match {
    case Done(result) => result
    case Even(value)  => run(even(value))
    case Odd(value)   => run(odd(value))
  }

  @Benchmark
  def bench() =
    if (run(even(3000))) "Yeah" else "Neh"
}

@State(Scope.Benchmark)
class ScalaTrampolineRunner {

  import scala.util.control.TailCalls.{ TailRec, done, tailcall }

  def even(i: Int): TailRec[Boolean] = i match {
    case 0 => done(true)
    case _ => tailcall(odd(i - 1))
  }

  def odd(i: Int): TailRec[Boolean] = i match {
    case 0 => done(false)
    case _ => tailcall(even(i - 1))
  }

  @Benchmark
  def bench() =
    if (even(3000).result) "Yeah" else "Neh"

}

@State(Scope.Benchmark)
class GeneralTrampolineRunner {

  sealed trait Computation[A]
  case class Continue[A](next: () => Computation[A]) extends Computation[A]
  case class Done[A](result: A) extends Computation[A]

  @inline def done[A](r: A) = Done(r)
  @inline def cont[A](r: => Computation[A]) = Continue(() => r)

  def even(i: Int): Computation[Boolean] = i match {
    case 0 => done(true)
    case _ => cont(odd(i - 1))
  }

  def odd(i: Int): Computation[Boolean] = i match {
    case 0 => done(false)
    case _ => cont(even(i - 1))
  }

  @tailrec
  final def run[A](computation: Computation[A]): A = computation match {
    case Done(a)        => a
    case c: Continue[A] => run(c.next())
  }

  @Benchmark
  def bench() =
    if (run(even(3000))) "Yeah" else "Neh"
}

@State(Scope.Benchmark)
class CatsTrampoline {

  import cats._
  import cats.implicits._
  import cats.free.Trampoline

  def even(i: Int): Trampoline[Boolean] = i match {
    case 0 => Trampoline.done(true)
    case _ => Trampoline.suspend(odd(i - 1))
  }

  def odd(i: Int): Trampoline[Boolean] = i match {
    case 0 => Trampoline.done(true)
    case _ => Trampoline.suspend(even(i - 1))
  }

  @Benchmark
  def bench() =
    if (even(3000).run) "Yeah" else "Neh"

}

@State(Scope.Benchmark)
class ExceptionTrampolines {

  import scala.util.control.ControlThrowable

  class Continue[A](val next: () => A) extends ControlThrowable

  @inline def cont[A](next: => A): A = throw new Continue(() => next)

  def even(i: Int): Boolean = i match {
    case 0 => true
    case _ => cont(odd(i - 1))
  }

  def odd(i: Int): Boolean = i match {
    case 0 => false
    case _ => cont(even(i - 1))
  }

  @tailrec
  final def run[A](c: => A): A =
    try c
    catch {
      case e: Continue[A] => run(e.next())
    }

  @Benchmark
  def bench() =
    if (run(even(3000))) "Yeah" else "Neh"
}

@State(Scope.Benchmark)
class BatchedTrampolines {

  val MaxDepth = 6000

  sealed trait Cont
  case class Done(r: Boolean) extends Cont
  case class Even(i: Int, d: Int) extends Cont
  case class Odd(i: Int, d: Int) extends Cont

  def even(i: Int, depth: Int): Cont =
    i match {
      case 0 =>
        Done(true)
      case _ =>
        if (depth >= MaxDepth)
          Odd(i - 1, depth + 1)
        else
          odd(i - 1, depth + 1)
    }

  def odd(i: Int, depth: Int): Cont =
    i match {
      case 0 =>
        Done(false)
      case _ =>
        if (depth >= MaxDepth)
          Even(i - 1, depth + 1)
        else
          even(i - 1, depth + 1)
    }

  def run[T](c: => Cont): Boolean = {
    var curr = c
    while (!curr.isInstanceOf[Done])
      curr = curr match {
        case _: Done => curr
        case e: Even => even(e.i, e.d)
        case o: Odd  => odd(o.i, o.d)
      }
    curr.asInstanceOf[Done].r
  }

  @Benchmark
  def bench() =
    if (run(even(3000, 0))) "Yeah" else "Neh"
}
