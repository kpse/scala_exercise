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
      def maximum(tree: Tree[Int]): Int = tree match {
        case Branch(l, r) => maximum(l) max maximum(r)
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
          case Branch(l, r) => inner(l, current + 1, (current + 1) max maxSoFar) max inner(r, current + 1, (current + 1) max maxSoFar)
        }
        inner(tree, 1, 1)
      }
      assert(depth(Leaf(1)) == 1)
      assert(depth(Branch(Leaf(2), Leaf(3))) == 2)
      assert(depth(Branch(Leaf(4), Branch(Leaf(1), Leaf(9)))) == 3)
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
  }
}
