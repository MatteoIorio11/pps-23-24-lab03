package u03

import u02.AnonymousFunctions.l
import u02.Modules.*
import u02.Modules.Person.getCourse
import u03.Optionals.Optional

import scala.annotation.tailrec
import scala.jdk.Accumulator
import scala.language.postfixOps

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

    // Lab 03
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Nil(), Nil()) => Nil()
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
      case (Cons(h1, t1), Nil()) => Nil()
      case (Nil(), Cons(h1, t1)) => Nil()



    def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
      case Nil() => Nil()
      case Cons(h, t) => {
        if (n > 0) {
          Cons(h, take(t)(n - 1))
        } else {
          Nil()
        }
      };

    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = (l1, l2) match
      case (Nil(), Nil()) => Nil()
      case (Nil(), Cons(h2, t2)) => Cons(h2, concat(Nil(), t2))
      case (Cons(h1, t1), Nil()) => Cons(h1, concat(t1, Nil()))
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1, concat(t1, l2))

    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
      case Nil() => Nil()
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))

    def mapFlat[A, B](l: Sequence[A])(map: A => B): Sequence[B] =
      flatMap(l)(x => Cons(map(x), Nil()))

    def filterFlat[A](l: Sequence[A])(pred: A => Boolean) = null


    def min(l: Sequence[Int]): Optional[Int] = _minTail(l, Int.MaxValue, Optional.Empty())
      @tailrec
      private def _minTail(l: Sequence[Int], minVal: Int, result: Optional[Int]): Optional[Int] = l match
        case Nil() => result
        case Cons(h, tail) => {
          if (h < minVal) {
            _minTail(tail, h, Optional.Just(h))
          }else{
            _minTail(tail, minVal, Optional.Just(minVal))
          }
        };

    def getCourses(s: Sequence[Person]): Sequence[String] =
      val teachers = filter(s)(x => !isStudent(x))
      map(teachers)(x => getCourse(x))

    def foldLeft(s: Sequence[Int])(defaultValue: Int)(accumulator: (Int, Int) => Int): Int = _calcValue(s, defaultValue)(accumulator)
    def _calcValue(s: Sequence[Int], result: Int)(accumulator: (Int, Int) => Int): Int = s match
      case Nil() => result
      case Cons(h, t) => _calcValue(t, accumulator(result, h))(accumulator)



@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
