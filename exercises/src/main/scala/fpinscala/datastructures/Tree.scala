package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  // Exercise 25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(lt, rt) => size(lt) + size(rt)
  }

  // Exercise 26
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(lt, rt) => maximum(lt).max(maximum(rt))
  }

  // Exercise 27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(lt, rt) => depth(lt).max(depth(rt)) + 1
  }

  // Exercise 28
  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(lt, rt) => Branch(map(lt)(f), map(rt)(f))
  }

  // Exercise 29
  def fold[A,B](tree: Tree[A])(lf_fn: A => B)(br_fn: (B,B) => B): B = tree match {
    case Leaf(v) => lf_fn(v)
    case Branch(lt, rt) => br_fn(fold(lt)(lf_fn)(br_fn), fold(rt)(lf_fn)(br_fn))
  }

  def sizeFld[A](tree: Tree[A]): Int =
    fold(tree) (_ => 1) (_ + _)

  def maximumFld(tree: Tree[Int]): Int =
    fold(tree) (v => v) (_.max(_))

  def depthFld[A](tree: Tree[A]): Int =
    fold(tree) (_ => 1) (_.max(_) + 1)

  def mapFld[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree) (v => Leaf(f(v)): Tree[B]) (Branch(_, _))
}
