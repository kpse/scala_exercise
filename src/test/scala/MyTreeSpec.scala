import org.scalatest.FunSpec

import scala.annotation.tailrec

class MyTreeSpec extends FunSpec {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  describe("My Tree") {
    // 3-25
    it("should count its node") {
      def size[A](tree: Tree[A]): Int = tree match {
        case Branch(l, r) => size(l) + size(r)
        case Leaf(v) => 1
      }
      assert(size(Leaf("a")) == 1)
      assert(size(Branch(Leaf("a"), Leaf("b"))) == 2)
      assert(size(Branch(Leaf("a"), Branch(Leaf("c"), Leaf("d")))) == 3)
    }

    // 3-26
    it("should count its maximum node") {
      def maximum[A](tree: Tree[A])(implicit ord: Ordering[A]): A = tree match {
        case Branch(l, r) => ord.max(maximum(l), maximum(r))
        case Leaf(v) => v
      }
      assert(maximum(Leaf(1)) == 1)
      assert(maximum(Branch(Leaf(2), Leaf(3))) == 3)
      assert(maximum(Branch(Leaf(4), Branch(Leaf(1), Leaf(9)))) == 9)
    }

    // 3-27
    it("should count its maximum depth") {
      def depth[A](tree: Tree[A]): Int = {
        def inner(branches: Tree[A], current: Int, maxSoFar: Int): Int = branches match {
          case Leaf(v) => maxSoFar
          case Branch(l, r) => 1 + (inner(l, current, current max maxSoFar) max inner(r, current, current max maxSoFar))
        }
        inner(tree, 1, 1)
      }
      assert(depth(Leaf(1)) == 1)
      assert(depth(Branch(Leaf(2), Leaf(3))) == 2)
      assert(depth(Branch(Leaf(4), Branch(Leaf(1), Leaf(9)))) == 3)
      assert(depth(Branch(Branch(Leaf(1), Leaf(1)), Branch(Branch(Leaf(1), Branch(Leaf(1), Leaf(1))), Leaf(9)))) == 5)
    }

    // 3-28
    it("should map over nodes") {
      def map[A](tree: Tree[A])(f: A => A): Tree[A] = tree match {
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
        case Leaf(v) => Leaf(f(v))
      }
      assert(map(Leaf(1))(_ + 1) == Leaf(2))
      assert(map(Branch(Leaf(2), Leaf(3)))(_ + 1) == Branch(Leaf(3), Leaf(4)))
      assert(map(Branch(Leaf(4), Branch(Leaf(1), Leaf(9))))(_ - 1) == Branch(Leaf(3), Branch(Leaf(0), Leaf(8))))
    }

    // 3-29
    it("should fold over nodes") {
      def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
        case Leaf(v) => f(v)
      }

      def mergeAdd(left: Int, right: Int): Int = left + right
      assert(fold(Leaf(1))(_ + 0)(mergeAdd) == 1)
      assert(fold(Branch(Leaf(2), Leaf(3)))(_ + 0)(mergeAdd) == 5)
      assert(fold(Branch(Leaf(4), Branch(Leaf(1), Leaf(9))))(_ + 0)(mergeAdd) == 14)

      def foldMaximum(v: Int): Int = v
      def mergeMaximum(left: Int, right: Int): Int = left max right

      assert(fold(Leaf(1): Tree[Int])(foldMaximum)(mergeMaximum) == 1)
      assert(fold(Branch(Leaf(2), Leaf(3)))(foldMaximum)(mergeMaximum) == 3)
      assert(fold(Branch(Leaf(4), Branch(Leaf(1), Leaf(9))))(foldMaximum)(mergeMaximum) == 9)

      def foldSize[A](v: A) = 1
      def mergeSize(left: Int, right: Int) = left + right

      assert(fold(Leaf(1): Tree[Int])(foldSize)(mergeSize) == 1)
      assert(fold(Branch(Leaf(2), Leaf(3)))(foldSize)(mergeSize) == 2)
      assert(fold(Branch(Leaf(4), Branch(Leaf(1), Leaf(9))))(foldSize)(mergeSize) == 3)

      def foldDepth[A](v: A) = 1
      def mergeDepth(left: Int, right: Int) = 1 + (left max right)

      assert(fold(Leaf(1): Tree[Int])(foldDepth)(mergeDepth) == 1)
      assert(fold(Branch(Leaf(2), Leaf(3)))(foldDepth)(mergeDepth) == 2)
      assert(fold(Branch(Branch(Leaf(1), Leaf(1)), Branch(Branch(Leaf(1), Branch(Leaf(1), Leaf(1))), Leaf(9))))(foldDepth)(mergeDepth) == 5)

      def foldMap(v: Int): Tree[Int] = Leaf(v + 1)
      def mergeMap(left: Tree[Int], right: Tree[Int]): Tree[Int] = Branch(left, right)

      assert(fold(Leaf(1): Tree[Int])(foldMap)(mergeMap) == Leaf(2))
      assert(fold(Branch(Leaf(2), Leaf(3)))(foldMap)(mergeMap) == Branch(Leaf(3), Leaf(4)))
      assert(fold(Branch(Branch(Leaf(1), Leaf(1)), Branch(Branch(Leaf(1), Branch(Leaf(1), Leaf(1))), Leaf(9))))(foldMap)(mergeMap) ==
        Branch(Branch(Leaf(2), Leaf(2)), Branch(Branch(Leaf(2), Branch(Leaf(2), Leaf(2))), Leaf(10))))

    }
  }
}
