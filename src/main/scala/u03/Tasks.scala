package u03

import u03.Sequences.Sequence.Nil
import u03.Sequences.Sequence.Cons
import u03.Sequences.Sequence

object Tasks:
    
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


