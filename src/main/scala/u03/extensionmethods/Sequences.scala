package u03.extensionmethods

import u03.Optionals.Optional

import scala.annotation.tailrec

object Sequences:
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    extension (l: Sequence[Int])
      def sum: Int = l match
        case Cons(h, t) => h + t.sum
        case _          => 0

    extension [A](l: Sequence[A])

      def map[B](mapper: A => B): Sequence[B] = l match
        case Cons(h, t) => Cons(mapper(h), t.map(mapper))
        case Nil()      => Nil()

      def filter(pred: A => Boolean): Sequence[A] = l match
        case Cons(h, t) if pred(h) => Cons(h, t.filter(pred))
        case Cons(_, t)            => t.filter(pred)
        case Nil()                 => Nil()

      def zip[B](second:  Sequence[B]): Sequence[(A, B)] = (l, second) match
        case (Nil(), Nil()) => Nil()
        case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), t1.zip(t2))
        case (Cons(h1, t1), Nil()) => Nil()
        case (Nil(), Cons(h2, t2)) => Nil()

      def take(n: Int): Sequence[A] = l match
        case Nil() => Nil()
        case Cons(h, t) if (n > 0) => Cons(h, t.take(n-1))
        case Cons(h, t) => Nil()

      def concat(l2: Sequence[A]): Sequence[A] = (l, l2) match
        case (Nil(), Nil()) => Nil()
        case (Nil(), Cons(h2, t2)) => Cons(h2, concat(t2))
        case (Cons(h1, t1), Nil()) => Cons(h1, t1.concat(Nil()))
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1, t1.concat(l2))

      def flatMap[B](mapper: A => Sequence[B]): Sequence[B] = l match
        case Nil() => Nil()
        case Cons(h, t) => mapper(h).concat(t.flatMap(mapper))

      def mapFlat[B](map: A => B): Sequence[B] =
        l.flatMap(x => Cons(map(x), Nil()))


    extension (s: Sequence[Int])
      def min(): Optional[Int] = _minTail(s, Int.MaxValue, Optional.Empty())
        @tailrec
        private def _minTail[A](l: Sequence[Int], minVal: Int, result: Optional[Int]): Optional[Int] = l match
          case Nil() => result
          case Cons(h, tail) =>
            if (h < minVal)
              _minTail(tail, h, Optional.Just(h))
            else
              _minTail(tail, minVal, Optional.Just(minVal))

      def foldLeft(defaultVal: Int)(accumlator: (Int, Int) => Int): Int = s match
        case Nil() => defaultVal
        case Cons(h, t) => t.foldLeft(h + defaultVal)(accumlator)


    def of[A](n: Int, a: A): Sequence[A] =
      if (n == 0) then Nil[A]() else Cons(a, of(n - 1, a))

@main def trySequences() =
  import Sequences.*
  import Sequence.*
  
  val seq = Cons(10, Cons(20, Cons(30, Nil())))
  println(seq.filter(_ >= 20).map(_ + 1).sum) // 21+31 = 52
  println(sum(map(filter(seq)(_ >= 20))(_ + 1))) // equally possible
  val seq2 = of(10, -1) // Cons(-1, Cons(-1, Cons(-1, ...)))
  println(seq2.sum) // -10
  
