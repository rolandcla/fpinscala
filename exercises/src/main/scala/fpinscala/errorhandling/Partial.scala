package fpinscala.errorhandling

// Exercise 9
sealed trait Partial[+A,+B] {
  def map[C](f: B => C): Partial[A,C] = this match {
    case Errors(es) => Errors(es)
    case Success(x) => Success(f(x))
  }

  def flatMap[AA >: A, C](f: B => Partial[AA, C]): Partial[AA, C] = this match {
    case Errors(x) => Errors(x)
    case Success(x) => f(x)
  }

  def orElse[AA >: A, C >: B](c: => Partial[AA, C]): Partial[AA, C] = this match {
    case Errors(_) => c
    case x => x
  }

  def map2[AA >: A, C, D](b: Partial[AA, C])(f: (B, C) => D): Partial[AA, D] =
    (this, b) match {
      case (Errors(es1), Errors(es2)) => Errors(es1 ++ es2)
      case (Errors(es), _)            => Errors(es)
      case (_, Errors(es))            => Errors(es)
      case (Success(s1), Success(s2)) => Success(f(s1, s2))
    }

}
case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
case class Success[+B](get: B) extends Partial[Nothing,B]

object Partial {
  def traverse[E,A,B](es: List[A])(f: A => Partial[E, B]): Partial[E, List[B]] =
    es.foldRight[Partial[E,List[B]]] (Success(List[B]())) ((a, acc) => f(a).map2(acc){_ :: _})

  def sequence[E,A](es: List[Partial[E,A]]): Partial[E,List[A]] =
    traverse(es)(x => x)
}
