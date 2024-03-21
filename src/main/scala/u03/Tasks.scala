package u03

import u03.Sequences.Sequence.Nil
import u03.Sequences.Sequence.Cons
import u03.Sequences.Sequence
import scala.annotation.tailrec
import u03.Optionals.Optional    
import u02.Modules.Person
import u03.Sequences.Sequence.filter
import u02.Modules.isStudent
import u03.Sequences.Sequence.map
import u02.Modules.Person.getCourse
import u03.Streams.Stream.cons
import u03.Streams.Stream.*

    object Task1:
        
        def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
            case Nil() => Nil()
            case Cons(h, t) =>
                if (n > 0) 
                    Cons(h, take(t)(n - 1))
                else
                    Nil()
        
        def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
            case (Nil(), Nil()) => Nil()
            case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
            case (Cons(h1, t1), Nil()) => Nil()
            case (Nil(), Cons(h1, t1)) => Nil()
        
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
        
        def filterFlat[A](l: Sequence[A])(pred: A => Boolean): Sequence[A] = l match
            case Nil() => Nil()
            case Cons(h, t) => flatMap(l)(x => if (pred(x)) Cons(x, Nil()) else  Nil())

    object Task2:
        def min(l: Sequence[Int]): Optional[Int] = _minTail(l, Int.MaxValue, Optional.Empty())
            @tailrec
            private def _minTail(l: Sequence[Int], minVal: Int, result: Optional[Int]): Optional[Int] = l match
                case Nil() => result
                case Cons(h, tail) =>
                    if (h < minVal)
                        _minTail(tail, h, Optional.Just(h))
                    else
                        _minTail(tail, minVal, Optional.Just(minVal))
    
    object Task3:
        def getCourses(s: Sequence[Person]): Sequence[String] =
            val teachers = filter(s)(x => !isStudent(x))
            map(teachers)(x => getCourse(x))

    object Task4:
        def foldLeft(s: Sequence[Int])(defaultValue: Int)(accumulator: (Int, Int) => Int): Int = _calcValue(s, defaultValue)(accumulator)
            @tailrec
            private def _calcValue(s: Sequence[Int], result: Int)(accumulator: (Int, Int) => Int): Int = s match
            case Nil() => result
            case Cons(h, t) => _calcValue(t, accumulator(result, h))(accumulator)
    
    object Task5:
        extension [A](l: Sequence[A])
            def map[B](mapper: A => B): Sequence[B] = l match
                case Cons(h, t) => Cons(mapper(h), t.map(mapper))
                case Nil()      => Nil()

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
            
            def filterFlat(pred: A => Boolean): Sequence[A] = l match            
                case Nil() => Nil()
                case Cons(h, t) => t.flatMap(x => if (pred(x)) Cons(x, Nil()) else  Nil())
        
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
            
        extension (s: Sequence[Person])
            def getCourses(): Sequence[String] =
                val teachers = filter(s)(x => !isStudent(x))
                s.map(x => getCourse(x))
    
    
    object Task6:
        def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
            case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
            case _ => Empty()        
    object Task7:
        def fill[A](strSize: => Int)(k: => A): Stream[A] = 
            if (strSize > 0)
                cons(k, fill(strSize - 1)(k))
            else
                Empty()





        
        

