package fpinscala.datastructures

import scala.collection.mutable.ListBuffer


sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of an empty list")
    case Cons(h, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("tail of an empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(h, t) => drop(t, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] =
    dropWhile(l, f)

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of an empty list")
    case Cons(h, Cons(_, Nil)) => Cons(h, Nil)
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = {
    @annotation.tailrec
    def go(remaining: List[A], count: Int): Int = l match {
      case Nil => count
      case Cons(h, t) => go(t, count + 1)
    }

    go(l, 0)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go(remaining: List[A], acc: B): B = l match {
      case Nil => acc
      case Cons(h, t) => go(t, f(acc, h))
    }

    go(l, z)
  }

  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft2(t, f(z, h))(f)
    }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil : List[A]) { (as, a) => Cons(a, as) }

  def flip[A, B, C](
    f: (A, B) => C
  ): (B, A) => C =
    (b, a) => f(a, b)

  def foldRightViaFoldLeft[A, B](
    l: List[A],
    z: B
  )(
    f: (A, B) => B
  ): B =
    foldLeft[A, B => B](l, identity) {  // Note: `identity` is `x => x`
      (g, a) => (b) => g(f(a,b))
    }(z)

  def foldLeftViaFoldRight[A, B](
    l: List[A],
    z: B
  )(
    f: (B, A) => B
  ): B =
    foldRight[A, B => B](l, identity) {
      (a: A, g: B => B) => (b: B) => g(f(b, a))
    }(z)

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(l, List[B]()) { (a, as) =>
      Cons(f(a), as)
    }

  // Note that this is still referentially transparent,
  // even though a mutable data structure is being used.
  def map2[A, B](l: List[A])(f: A => B): List[B] = {
    val buf = new ListBuffer[B]
    def loop(remaining: List[A]): Unit = remaining match {
      case Nil => ()
      case Cons(h, t) =>
        buf += f(h)
        loop(t)
    }

    loop(l)
    List(buf.toList: _*)
  }

  def appendFold[A](l1: List[A], l2: List[A]): List[A] =
    foldRightViaFoldLeft(l1, l2)(Cons(_, _))

  def flatten[A](l: List[List[A]]): List[A] =
    foldRightViaFoldLeft(l, List[A]())(appendFold)

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, List[A]()) { (a, as) =>
      if (f(a)) Cons(a, as)
      else as
    }

  /* Various flatMap implementations */

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    flatten(map(l)(f))

  def flatMap2[A, B](l: List[A])(f: A => List[B]): List[B] = {
    @annotation.tailrec
    def go(remaining: List[A], acc: List[B]): List[B] =
      remaining match {
        case Nil => acc
        case Cons(h, t) => go(t, append(acc, f(h)))
      }

    go(l, Nil)
  }

  def flatMap3[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, List[B]()) { (a, acc) =>
      append(f(a), acc)
    }



  /* hasSubsequence */

  @annotation.tailrec
  def startsWith[A](l: List[A], s: List[A]): Boolean =
    (l, s) match {
      case (_, Nil) => true
      case (Cons(lh, lt), Cons(sh, st)) if (lh == sh) => startsWith(lt, st)
      case _ => false
    }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup match {
      case Nil => false
      case Cons(h, t) if startsWith(sup, sub) => true
      case Cons(h, t) =>  hasSubsequence(t, sub)
    }
  }


  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

  def addWith(as: List[Int], bs: List[Int]): List[Int] =
    zipWith(as, bs) { _ + _ }

}
