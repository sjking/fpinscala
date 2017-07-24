package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  //            O
  //          /   \
  //         O     O
  //        / \   / \
  //       L   L L   L
  val ex1: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

  //                  O
  //                /   \
  //               O     L
  //              / \
  //             O   L
  //            / \
  //           L   L
  val ex2: Tree[Int] = Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(2)), Leaf(3))

  // 3.25: counts how many nodes in a tree
  def size[A](tree: Tree[A]): Int = {
    def sizeR(tree: Tree[A], acc: Int): Int = tree match {
      case Leaf(_) => acc + 1
      case Branch(l, r) => sizeR(l, acc) + sizeR(r, acc) + 1
    }
    sizeR(tree, 0)
  }

  // 3.26: Maximum returns maximum element in a Tree[Int]
  def maximum(tree: Tree[Int]): Int = {
    def maxR(tree: Tree[Int], maxVal: Option[Int]): Int = tree match {
      case Leaf(x) => maxVal match {
        case Some(m) => x.max(m)
        case None => x
      }
      case Branch(l, r) => maxR(l, maxVal) max maxR(r, maxVal)
    }
    maxR(tree, None)
  }

  // 3.27: Returns maximum path length from root to any leaf
  def maximumPathLength[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => (maximumPathLength(l) max maximumPathLength(r)) + 1
  }

  // 3.28: Write a function map, analogous to the method of the same name on List, that modi- fies each element in a
  // tree with a given function.
  def map[A](tree: Tree[A])(f: A => A): Tree[A] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // 3.29
  def fold[A,B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeFold[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((x, y) => x + y + 1)

  def maximumFold(tree: Tree[Int]): Int = fold(tree)(x => x)((x, y) => x.max(y))

  def depthFold[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((x, y) => x.max(y) + 1)

  def mapFold[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)((a: A) => Leaf(f(a)): Tree[B])((a: Tree[B], b: Tree[B]) => Branch(a, b))
}