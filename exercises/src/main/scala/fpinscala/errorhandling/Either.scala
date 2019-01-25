package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case l@Left(e) => l
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case l@Left(_) => l
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(_) => this
    case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    a_ <- this
    b_ <- b
  } yield (f(a_, b_))
}

case class Left[+E](get: E) extends Either[E,Nothing] {
  override def map[B](f: Nothing => B): Either[E, B] = this
  override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = this
  override def orElse[EE >: E, B](b: => Either[EE, B]): Either[EE, B] = b
  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this
}

case class Right[+A](get: A) extends Either[Nothing,A] {
  override def map[B](f: A => B): Either[Nothing, B] = Right(f(get))
  override def flatMap[EE, B](f: A => Either[EE, B]): Either[EE, B] = f(get)
  override def orElse[EE, B >: A](b: => Either[EE, B]): Either[EE, B] = this
  override def map2[EE, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    b.map(f(get, _))
}

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]](Right(Nil)) { (a, bs) => f(a).map2(bs)(_ :: _) }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def insuranceRateQuote(age:Int, tickets: Int): Double = ???

  def parseInsuranceRateQuote(
    age: String,
    numberOfSpeedingTickets: String): Either[Exception,Double] =
  for {
    a <- Try { age.toInt }
    tickets <- Try { numberOfSpeedingTickets.toInt }
  } yield insuranceRateQuote(a, tickets)
}
