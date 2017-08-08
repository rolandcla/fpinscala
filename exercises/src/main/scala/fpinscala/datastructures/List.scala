package fpinscala.datastructures

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

  // Exercise 1
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
  // == 3 (tested in ListSpec.scala)

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

  // Exercise 2
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => throw new NoSuchElementException("tail(Nil)")
      case Cons(_, t) => t
    }

  // Exercise 5
  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => throw new NoSuchElementException("setHead(Nil)")
      case Cons(_, t) => Cons(h, t)
    }

  // Exercise 3
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Nil => throw new NoSuchElementException("drop(Nil,_)")
      case Cons(_, t) => drop(t, n - 1)
    }

  // Exercise 4
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t, f)
                         else l
    }

  // Exercise 6
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => throw new NoSuchElementException("init(Nil)")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  // Exercise 7
  def product3(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  // Exercise 8
  // tested in ListSpec.scala !!!

  // Exercise 9
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, n) => n + 1)

  // Exercise 10
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  // Exercise 11
  def sumFL(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)

  def productFL(xs: List[Double]): Double = foldLeft(xs, 1.0)(_ * _)

  def lengthFL[A](xs: List[A]): Int = foldLeft(xs, 0)((l,_) => l + 1)

  // Exercise 12
  def reverse[A](xs: List[A]): List[A] = foldLeft(xs, Nil: List[A])( (ys, y) => Cons(y, ys) )

  // Exercise 13
  def foldRightUsingFL[A,B](l: List[A], z: B)(f: (A, B) => B) =
    foldLeft(reverse(l), z)( (b,a) => f(a, b) )

  // def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
  //   as match {
  //     case Nil => z
  //     case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  //   }

  // def foldLeftUsingFR[A,B](l: List[A], z: B)(f: (B, A) => B) = {
  //   def go(xxs: List[A]): B => B = xxs match {
  //     case Nil         => (acc: B) => acc
  //     case Cons(x, xs) => (acc: B) => go(xs)(f(acc, x))
  //   }
  //   go(l)(z)
  // }

  def foldLeftUsingFR[A,B](l: List[A], z: B)(f: (B, A) => B) = {
    def step(x: A, facc: B => B): B => B = (acc: B) => facc(f(acc, x))
    foldRight(l, (acc: B) => acc)(step)(z)
  }

  // Exercise 14
  def appendUsingFR[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((a: A, aa: List[A]) => Cons(a, aa))

  // Exercise 15
  def concatenate[A](xss: List[List[A]]): List[A] =
    foldRight(xss, Nil: List[A])((xs, acc) => appendUsingFR(xs, acc))


  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}
