package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (h, t) =>
      if (p(h)) cons(h, t) else empty
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true) { (a, acc) =>
      p(a) && acc
    }

  def headOption: Option[A] =
    foldRight(None: Option[A]) { (h, _) =>  Some(h) }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B]) { (h, t) =>
      cons(f(h), t)
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B>:A](other: => Stream[B]): Stream[B] =
    foldRight(other)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(h, t) => h() :: t().toList
  }

  def mapUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()) -> t())
      case _ => None
    }

  def takeUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some(h(), (empty, 0))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if (p(h())) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](other: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, other)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zip[B](other: Stream[B]): Stream[(A, B)] =
    zipWith(other)((_, _))

  def zipWithAllUnfold[B, C](other: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, other)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), Empty) => Some((f(Some(h1()), None), (t1(), empty)))
      case (Empty, Cons(h2, t2)) => Some((f(None, Some(h2())), (empty, t2())))
      case _ => None
    }

  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAllUnfold(other)((_, _))

  def startsWith[B >: A](sub: Stream[B]): Boolean =
    zip(sub).forAll { case (a, b) => a == b }

  def tails: Stream[Stream[A]] =
    scanRight(empty[A])(cons[A](_, _))

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  // This is no good - it runs multiple times on each element
  def scanRightMultipleTimes[B](z: B)(f: (A, => B) => B): Stream[B] = this match {
    case Cons(_, t) => cons(this.foldRight(z)(f), t().scanRight(z)(f))
    case _ => cons(z, empty)
  }

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight(cons(z, empty)) {
      case (item, c@Cons(h, _)) => cons(f(item, h()), c)
      case _ => sys.error("This shouldn't happen")
    }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def fibs: Stream[Int] = {
    def loop(a: Int, b: Int): Stream[Int] =
      cons(a, loop(b, a + b))

    loop(1, 1)
  }


  def fibsUnfold: Stream[Int] = unfold((1, 1)) { case (a, b) => Some(a -> (b, a + b)) }
  def constantUnfold[A](a: A): Stream[A] = unfold(a) { _ => Some(a, a) }
  def fromUnfold(n: Int): Stream[Int] = unfold(n) { n => Some(n, n + 1) }
  def onesUnfold: Stream[Int] = unfold(1) { _ => Some(1, 1) }




































 def maybeTwice2(b: Boolean, i: => Int) = {
   lazy val j = i
   if (b) {
     j+j
   } else 0
 }

}
