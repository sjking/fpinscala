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
    case Cons(_, xs) => xs
    case Nil => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, xs) => Cons(h, xs)
    case Nil => Nil
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else drop(tail(l), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(y, ys) =>
      if (f(y)) dropWhile(ys, f)
      else Cons(y, ys)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Nil => Nil
    case Cons(y, ys) => Cons(y, init(ys))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, len) => len + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    def foldL(ys: List[A], acc: B): B = ys match {
      case Nil => acc
      case Cons(x, xs) => foldL(xs, f(acc, x))
    }
    foldL(l, z)
  }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((x,_) => x + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((bs: List[A], a) => Cons(a, bs))

  // Exercise 3.13: foldLeft in terms of foldRight, and foldRight in terms of foldLeft
  def foldLeftR[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def foldRightL[A,B](as: List[A], z: B)(f: (A, B) => B): B = ???

  // 3.14: Append in terms of foldLeft or foldRight
  def appendFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((a: A, bs: List[A]) => Cons(a, bs))

  // 3.15: Hard: Write a function that concatenates a list of lists into a single list. Its runtime should be linear in
  // the total length of all lists. Try to use functions we have already defined.
  def concatenateListOfLists[A](lists: List[List[A]]): List[A] = {
    foldLeft(lists, Nil: List[A])((flattened: List[A], a: List[A]) => append(flattened, a))
  }

  // 3.16: Write a function that transforms a list of integers by adding 1 to each element. (Reminder: this should be a
  // pure function that returns a new List!)
  def addOneToEach(ints: List[Int]): List[Int] = foldRight(ints, Nil: List[Int])((a, ys) => Cons(a + 1, ys))

  // 3.17: Write a function that turns each value in a List[Double] into a String. You can use the expression d.toString
  // to convert some d: Double to a String.
  def doublesToStrings(numbers: List[Double]): List[String] =
    foldRight(numbers, Nil: List[String])((a, strs) => Cons(a.toString, strs))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((a, bs) => Cons(f(a), bs))

  // 3.19: Write a function filter that removes elements from a list unless they satisfy a given predicate. Use it to
  // remove all odd numbers from a List[Int].
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, bs) => if(f(a)) Cons(a, bs) else bs)

  def removeOdds(nums: List[Int]): List[Int] = filter(nums)(_ % 2 != 0)

  // 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((a, bs) => append(f(a), bs))

  // 3.21: Filter implemented with flatMap
  def filterFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  // 3.22: Write a function that accepts two lists and constructs a new list by adding correspond- ing elements. For
  // example, List(1,2,3) and List(4,5,6) become List(5,7,9)
  def head[A](l: List[A]): A = l match {
    case Cons(y, _) => y
  }
  def addCorrespondingElements(as: List[Int], bs: List[Int]): List[Int] = {
    val (result, _) = foldLeft(as, (Nil: List[Int], bs)) { (cs, a) =>
      cs match {
        case (acc, rem) => Cons(a + head(rem), acc) -> tail(rem)
      }
    }
    reverse(result)
  }

  // 3.23: Generalize the function you just wrote so that it’s not specific to integers or addition. Name your
  // generalized function zipWith.
  def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] = {
    val (result, _) = foldLeft(as, (Nil: List[A], bs)) { (cs, a) =>
      cs match {
        case (acc, rem) => Cons(f(a, head(rem)), acc) -> tail(rem)
      }
    }
    reverse(result)
  }

}
